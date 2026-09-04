Class {
	#name : 'YuukiCodexTest',
	#superclass : 'TestCase',
	#category : 'Yuuki-Tests'
}

{ #category : 'helpers' }
YuukiCodexTest >> fakeJwt: payloadJson [
	| encoded |
	encoded := (ZnBase64Encoder new encode: payloadJson utf8Encoded) copyReplaceAll: '+' with: '-'.
	encoded := (encoded copyReplaceAll: '/' with: '_') copyWithout: $=.
	^ 'eyJhbGciOiJub25lIn0.' , encoded , '.sig'
]

{ #category : 'tests' }
YuukiCodexTest >> testClassify [
	self assert: (YuukiCodex classify: (STON fromString: '{"type":"response.output_text.delta","delta":"hi"}')) equals: {#text. 'hi'}.
	self assert: (YuukiCodex classify: (STON fromString: '{"type":"response.completed","response":{"status":"completed"}}')) equals: {#finish. #stop}.
	self assert: (YuukiCodex classify: (STON fromString: '{"type":"response.incomplete","response":{"status":"incomplete","incomplete_details":{"reason":"max_output_tokens"}}}')) equals: {#finish. #length}.
	self assert: (YuukiCodex classify: (STON fromString: '{"type":"response.failed"}')) equals: {#finish. #failed}.
	self assert: (YuukiCodex classify: (STON fromString: '{"type":"response.created"}')) first isNil
]

{ #category : 'tests' }
YuukiCodexTest >> testJwtPayload [
	| token payload |
	token := self fakeJwt: '{"https://api.openai.com/auth":{"chatgpt_account_id":"acct_1"},"exp":1700000000}'.
	payload := YuukiCodex jwtPayload: token.
	self assert: (payload at: 'exp') equals: 1700000000.
	self assert: ((payload at: 'https://api.openai.com/auth') at: 'chatgpt_account_id') equals: 'acct_1'
]

{ #category : 'tests' }
YuukiCodexTest >> testReduceSse [
	| sample events result |
	sample := String streamContents: [ :s |
		s nextPutAll: 'event: response.created'; lf.
		s nextPutAll: 'data: {"type":"response.created","response":{"id":"r1"}}'; lf; lf.
		s nextPutAll: 'data: {"type":"response.output_item.done","output_index":0,"item":{"type":"reasoning","id":"rs1","encrypted_content":"ZZZ"}}'; lf; lf.
		s nextPutAll: 'data: {"type":"response.output_text.delta","delta":"working"}'; lf; lf.
		s nextPutAll: 'data: [DONE]'; lf.
		s nextPutAll: 'data: {"type":"response.output_item.done","output_index":1,"item":{"type":"function_call","call_id":"c1","name":"smalltalk","arguments":"{\"code\":\"1\"}"}}'; lf; lf.
		s nextPutAll: 'data: {"type":"response.completed","response":{"status":"completed"}}'; lf.
		s nextPutAll: 'data: {"type":"response.never"}'; lf ].
	events := OrderedCollection new.
	result := YuukiCodex reduceSse: sample readStream emit: [ :e | events add: e ].
	self assert: result second equals: #stop.
	self assert: result first size equals: 2.
	self assert: ((result first at: 1) at: 'type') equals: 'reasoning'.
	self assert: ((result first at: 2) at: 'call_id') equals: 'c1'.
	self assert: events asArray equals: {{#text. 'working'}}.
	self assert: (YuukiCodex reduceSse: 'data: {"type":"response.output_text.delta","delta":"x"}' readStream emit: [ :e | ]) second equals: #failed
]

{ #category : 'tests' }
YuukiCodexTest >> testRequestBody [
	| body |
	body := STON fromString: (YuukiCodex requestBody: 'SYS' items: {YuukiAgent userItem: 'hi'}).
	self assert: (body at: 'store') equals: false.
	self assert: (body at: 'stream') equals: true.
	self assert: (body at: 'instructions') equals: 'SYS'.
	self assert: (body at: 'input') size equals: 1.
	self assert: (((body at: 'tools') at: 1) at: 'name') equals: 'smalltalk'.
	self assert: ((body at: 'reasoning') at: 'effort') equals: YuukiCodex effort.
	self deny: (body includesKey: 'max_output_tokens')
]
