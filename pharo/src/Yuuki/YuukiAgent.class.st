"
One turn: history plus prompt, streamed, tool calls run until the model stops.
"
Class {
	#name : 'YuukiAgent',
	#superclass : 'Object',
	#category : 'Yuuki'
}

{ #category : 'items' }
YuukiAgent class >> callsIn: items [
	^ items select: [ :item | (item at: 'type' ifAbsent: [ nil ]) = 'function_call' ]
]

{ #category : 'configuration' }
YuukiAgent class >> maxSteps [
	^ 100
]

{ #category : 'items' }
YuukiAgent class >> outputItem: call output: output [
	^ Dictionary new
		at: 'type' put: 'function_call_output';
		at: 'call_id' put: (call at: 'call_id');
		at: 'output' put: output;
		yourself
]

{ #category : 'running' }
YuukiAgent class >> runCall: call emit: emit approve: approve [
	"Announce, approve and run one function_call; answer its function_call_output item."
	| id arguments code timeout output |
	id := call at: 'call_id'.
	arguments := [ STON fromString: (call at: 'arguments') ] on: Error do: [ :e | nil ].
	code := (arguments isDictionary and: [ (arguments at: 'code' ifAbsent: [ nil ]) isString ])
		ifTrue: [ arguments at: 'code' ]
		ifFalse: [ nil ].
	timeout := (code notNil and: [ (arguments at: 'timeout' ifAbsent: [ nil ]) isNumber ])
		ifTrue: [ arguments at: 'timeout' ]
		ifFalse: [ 60 ].
	emit value: {#call. id. code ifNil: [ '' ]}.
	output := code isNil
		ifTrue: [ 'error: malformed arguments, expected {"code": string}' ]
		ifFalse: [
			(approve value: id value: code)
				ifTrue: [ YuukiTool run: code timeout: timeout ]
				ifFalse: [ 'denied by user' ] ].
	emit value: {#result. id. output}.
	^ self outputItem: call output: output
]

{ #category : 'running' }
YuukiAgent class >> runTurn: history prompt: prompt emit: emit approve: approve [
	^ self
		runTurn: history prompt: prompt emit: emit approve: approve
		stream: [ :instructions :items :e | YuukiCodex streamTurn: instructions items: items emit: e ]
]

{ #category : 'running' }
YuukiAgent class >> runTurn: history prompt: prompt emit: emit approve: approve stream: stream [
	"Answer the new history. STREAM is a block of instructions, items and emit answering {outputItems. finish}."
	| items |
	items := OrderedCollection withAll: history.
	items add: (self userItem: prompt).
	self maxSteps timesRepeat: [ | result output finish calls |
		result := stream value: YuukiContext instructions value: items asArray value: emit.
		output := result first.
		finish := result second.
		items addAll: output.
		calls := self callsIn: output.
		(calls isEmpty or: [ finish ~= #stop ]) ifTrue: [ ^ items asArray ].
		calls do: [ :call | items add: (self runCall: call emit: emit approve: approve) ] ].
	items add: (self userItem: 'Step limit reached. Stop and summarize what you did.').
	^ items asArray
]

{ #category : 'items' }
YuukiAgent class >> userItem: text [
	^ Dictionary new
		at: 'role' put: 'user';
		at: 'content' put: (Array with: (Dictionary new at: 'type' put: 'input_text'; at: 'text' put: text; yourself));
		yourself
]
