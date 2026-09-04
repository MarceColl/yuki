"
The Codex provider: fx's or the codex CLI's session file, one streaming Responses request, SSE reduced to output items.
"
Class {
	#name : 'YuukiCodex',
	#superclass : 'Object',
	#classInstVars : [
		'model',
		'effort'
	],
	#category : 'Yuuki'
}

{ #category : 'session' }
YuukiCodex class >> authPath [
	| codex |
	codex := FileLocator home / '.codex' / 'auth.json'.
	^ codex exists
		ifTrue: [ codex ]
		ifFalse: [ FileLocator home / '.fx' / 'chatgpt-auth.json' ]
]

{ #category : 'stream' }
YuukiCodex class >> classify: event [
	"Answer {kind. payload} for one decoded SSE event; kind is nil for events we ignore."
	| type |
	type := event at: 'type' ifAbsent: [ '' ].
	(#('response.output_text.delta' 'response.refusal.delta') includes: type)
		ifTrue: [ ^ {#text. event at: 'delta'} ].
	(#('response.reasoning_summary_text.delta' 'response.reasoning_text.delta') includes: type)
		ifTrue: [ ^ {#reasoning. event at: 'delta'} ].
	type = 'response.reasoning_summary_part.done'
		ifTrue: [ ^ {#reasoning. String lf , String lf} ].
	type = 'response.output_item.done'
		ifTrue: [ ^ {#item. event at: 'item'} ].
	(#('response.completed' 'response.done' 'response.incomplete') includes: type)
		ifTrue: [ ^ {#finish. self finishReason: (event at: 'response' ifAbsent: [ nil ])} ].
	(#('response.failed' 'error') includes: type)
		ifTrue: [ ^ {#finish. #failed} ].
	^ {nil. nil}
]

{ #category : 'configuration' }
YuukiCodex class >> effort [
	^ effort ifNil: [ 'high' ]
]

{ #category : 'configuration' }
YuukiCodex class >> effort: aString [
	effort := aString
]

{ #category : 'stream' }
YuukiCodex class >> finishReason: response [
	| status details reason |
	response ifNil: [ ^ #failed ].
	status := response at: 'status' ifAbsent: [ nil ].
	details := response at: 'incomplete_details' ifAbsent: [ nil ].
	reason := details ifNil: [ nil ] ifNotNil: [ :d | d at: 'reason' ifAbsent: [ nil ] ].
	status = 'completed' ifTrue: [ ^ #stop ].
	(status = 'incomplete' and: [ reason = 'max_output_tokens' ]) ifTrue: [ ^ #length ].
	(status = 'incomplete' and: [ reason = 'content_filter' ]) ifTrue: [ ^ #contentFilter ].
	status = 'incomplete' ifTrue: [ ^ #stop ].
	^ #failed
]

{ #category : 'session' }
YuukiCodex class >> jwtPayload: token [
	"The decoded payload object of a JWT."
	| part padded bytes |
	part := ((token substrings: '.') at: 2) copyReplaceAll: '-' with: '+'.
	part := part copyReplaceAll: '_' with: '/'.
	padded := part , (String new: (4 - (part size \\ 4)) \\ 4 withAll: $=).
	bytes := ZnBase64Encoder new decode: padded.
	^ STON fromString: (ZnUTF8Encoder new decodeBytes: bytes)
]

{ #category : 'configuration' }
YuukiCodex class >> model [
	^ model ifNil: [ 'gpt-5.6-luna' ]
]

{ #category : 'configuration' }
YuukiCodex class >> model: aString [
	model := aString
]

{ #category : 'stream' }
YuukiCodex class >> reduceSse: lines emit: emit [
	"LINES is a character read stream of SSE lines. Answer {outputItems. finish}."
	| output finish |
	output := OrderedCollection new.
	finish := nil.
	[ lines atEnd or: [ finish notNil ] ] whileFalse: [ | line |
		line := lines nextLine.
		(line notNil and: [ line beginsWith: 'data:' ]) ifTrue: [ | data |
			data := (line allButFirst: 5) trimBoth.
			(data beginsWith: '{') ifTrue: [ | kind |
				kind := self classify: (STON fromString: data).
				(#(text reasoning) includes: kind first) ifTrue: [ emit value: {kind first. kind second} ].
				kind first = #item ifTrue: [ output add: kind second ].
				kind first = #finish ifTrue: [ finish := kind second ] ] ] ].
	^ {output asArray. finish ifNil: [ #failed ]}
]

{ #category : 'request' }
YuukiCodex class >> requestBody: instructions items: items [
	^ STON toJsonString: (Dictionary new
		at: 'model' put: self model;
		at: 'store' put: false;
		at: 'stream' put: true;
		at: 'instructions' put: instructions;
		at: 'input' put: items asArray;
		at: 'tools' put: (Array with: self tool);
		at: 'tool_choice' put: 'auto';
		at: 'parallel_tool_calls' put: true;
		at: 'include' put: #('reasoning.encrypted_content');
		at: 'text' put: (Dictionary new at: 'verbosity' put: 'low'; yourself);
		at: 'reasoning' put: (Dictionary new at: 'effort' put: self effort; at: 'summary' put: 'auto'; yourself);
		yourself)
]

{ #category : 'session' }
YuukiCodex class >> session [
	"The session in one shape: access_token, refresh_token, account_id, expires_at_ms."
	| file tokens |
	file := STON fromString: self authPath contents.
	tokens := file at: 'tokens' ifAbsent: [ nil ].
	tokens ifNil: [ ^ file ].
	^ Dictionary new
		at: 'access_token' put: (tokens at: 'access_token');
		at: 'refresh_token' put: (tokens at: 'refresh_token');
		at: 'account_id' put: (tokens at: 'account_id');
		at: 'expires_at_ms' put: ((self jwtPayload: (tokens at: 'access_token')) at: 'exp') * 1000;
		yourself
]

{ #category : 'stream' }
YuukiCodex class >> streamTurn: instructions items: items emit: emit [
	"POST one Responses request; EMIT receives {#text. delta} and {#reasoning. delta}. Answer {outputItems. finish}."
	| session client chars |
	session := self session.
	client := ZnClient new.
	client
		url: 'https://chatgpt.com/backend-api/codex/responses';
		headerAt: 'Authorization' put: 'Bearer ' , (session at: 'access_token');
		headerAt: 'chatgpt-account-id' put: (session at: 'account_id');
		headerAt: 'originator' put: 'yuuki';
		headerAt: 'OpenAI-Beta' put: 'responses=experimental';
		accept: 'text/event-stream';
		entity: (ZnEntity json: (self requestBody: instructions items: items));
		streaming: true;
		timeout: 600;
		post.
	client isSuccess ifFalse: [
		self error: 'codex ' , client response status printString , ': ' , client contents asString ].
	chars := ZnCharacterReadStream on: client response entity stream encoding: 'utf8'.
	^ [ self reduceSse: chars emit: emit ] ensure: [ client close ]
]

{ #category : 'request' }
YuukiCodex class >> tool [
	^ Dictionary new
		at: 'type' put: 'function';
		at: 'name' put: 'smalltalk';
		at: 'strict' put: false;
		at: 'description' put: 'Evaluate Pharo Smalltalk in the live image. The code is compiled and run; the value of the last expression and anything shown on the Transcript come back. Classes and methods you define stay in the image. Build the helpers you need as methods in the package Yuuki-User and reuse them.';
		at: 'parameters' put: (Dictionary new
			at: 'type' put: 'object';
			at: 'properties' put: (Dictionary new
				at: 'code' put: (Dictionary new at: 'type' put: 'string'; yourself);
				at: 'timeout' put: (Dictionary new at: 'type' put: 'integer'; at: 'description' put: 'seconds before the evaluation is stopped, default 60'; yourself);
				yourself);
			at: 'required' put: #('code');
			yourself);
		yourself
]
