Class {
	#name : 'YuukiAgentTest',
	#superclass : 'TestCase',
	#instVars : [
		'seen'
	],
	#category : 'Yuuki-Tests'
}

{ #category : 'helpers' }
YuukiAgentTest >> callItem: id code: code [
	^ Dictionary new
		at: 'type' put: 'function_call';
		at: 'call_id' put: id;
		at: 'name' put: 'smalltalk';
		at: 'arguments' put: (STON toJsonString: (Dictionary new at: 'code' put: code; yourself));
		yourself
]

{ #category : 'helpers' }
YuukiAgentTest >> fakeStream: responses [
	"A stream block popping one canned {items. finish} per call, recording each request in seen."
	| queue |
	queue := OrderedCollection withAll: responses.
	seen := OrderedCollection new.
	^ [ :instructions :items :emit | | next |
		seen add: items.
		next := queue removeFirst.
		next first do: [ :item |
			(item at: 'type') = 'message' ifTrue: [ emit value: {#text. (((item at: 'content') at: 1) at: 'text')} ] ].
		next ]
]

{ #category : 'helpers' }
YuukiAgentTest >> messageItem: text [
	^ Dictionary new
		at: 'type' put: 'message';
		at: 'role' put: 'assistant';
		at: 'status' put: 'completed';
		at: 'content' put: (Array with: (Dictionary new at: 'type' put: 'output_text'; at: 'text' put: text; yourself));
		yourself
]

{ #category : 'tests' }
YuukiAgentTest >> testAppendsUserAndOutput [
	| events history |
	events := OrderedCollection new.
	history := YuukiAgent runTurn: #() prompt: 'hi'
		emit: [ :e | events add: e ]
		approve: [ :id :code | true ]
		stream: (self fakeStream: {{{self messageItem: 'hello'}. #stop}}).
	self assert: history size equals: 2.
	self assert: ((history at: 1) at: 'role') equals: 'user'.
	self assert: ((history at: 2) at: 'type') equals: 'message'.
	self assert: seen size equals: 1.
	self assert: (events includes: {#text. 'hello'})
]

{ #category : 'tests' }
YuukiAgentTest >> testDeniedCall [
	| history |
	history := YuukiAgent runTurn: #() prompt: 'rm'
		emit: [ :e | ]
		approve: [ :id :code | false ]
		stream: (self fakeStream: {{{self callItem: 'c1' code: 'Smalltalk'}. #stop}. {{self messageItem: 'ok'}. #stop}}).
	self assert: ((history at: 3) at: 'output') equals: 'denied by user'
]

{ #category : 'tests' }
YuukiAgentTest >> testRunsApprovedCallsAndLoops [
	| events history |
	events := OrderedCollection new.
	history := YuukiAgent runTurn: #() prompt: 'add'
		emit: [ :e | events add: e ]
		approve: [ :id :code | true ]
		stream: (self fakeStream: {{{self callItem: 'c1' code: '3 + 4'}. #stop}. {{self messageItem: 'done'}. #stop}}).
	self assert: history size equals: 4.
	self assert: ((history at: 3) at: 'type') equals: 'function_call_output'.
	self assert: (((history at: 3) at: 'output') includesSubstring: '=> 7').
	self assert: seen size equals: 2.
	self assert: (events includes: {#call. 'c1'. '3 + 4'})
]

{ #category : 'tests' }
YuukiAgentTest >> testStepLimitAppendsNotice [
	| history calls |
	calls := (1 to: YuukiAgent maxSteps) collect: [ :i | {{self callItem: 'c' , i printString code: '1'}. #stop} ].
	history := YuukiAgent runTurn: #() prompt: 'loop'
		emit: [ :e | ]
		approve: [ :id :code | true ]
		stream: (self fakeStream: calls , {{{self messageItem: 'never'}. #stop}}).
	self assert: seen size equals: YuukiAgent maxSteps.
	self assert: ((((history last at: 'content') at: 1) at: 'text') includesSubstring: 'Step limit')
]
