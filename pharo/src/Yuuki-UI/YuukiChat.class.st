"
A minimal chat window: transcript above, prompt below, approvals as dialogs.
"
Class {
	#name : 'YuukiChat',
	#superclass : 'SpPresenter',
	#instVars : [
		'transcript',
		'input',
		'history',
		'running'
	],
	#category : 'Yuuki-UI'
}

{ #category : 'opening' }
YuukiChat class >> open [
	^ self new open
]

{ #category : 'transcript' }
YuukiChat >> append: aString [
	UIManager default defer: [ transcript text: transcript text asString , aString ]
]

{ #category : 'layout' }
YuukiChat >> defaultLayout [
	^ SpBoxLayout newTopToBottom
		add: transcript;
		add: input expand: false;
		yourself
]

{ #category : 'events' }
YuukiChat >> handle: event [
	event first = #text ifTrue: [ ^ self append: event second ].
	event first = #call ifTrue: [ ^ self append: String lf , event third , String lf ].
	event first = #result ifTrue: [ ^ self append: event third , String lf ].
	^ nil
]

{ #category : 'initialization' }
YuukiChat >> initializePresenters [
	transcript := self newText beNotEditable; yourself.
	input := self newTextInput placeholder: 'ask yuuki'; yourself.
	input whenSubmitDo: [ :text | self submit: text asString ].
	history := #().
	running := false
]

{ #category : 'initialization' }
YuukiChat >> initializeWindow: aWindowPresenter [
	aWindowPresenter title: 'yuuki'; initialExtent: 900 @ 600
]

{ #category : 'running' }
YuukiChat >> submit: prompt [
	prompt trimBoth isEmpty ifTrue: [ ^ self ].
	running ifTrue: [ ^ self append: '(busy)' , String lf ].
	running := true.
	input text: ''.
	self append: '> ' , prompt , String lf.
	[ [ history := YuukiAgent
			runTurn: history prompt: prompt
			emit: [ :event | self handle: event ]
			approve: [ :id :code | UIManager default confirm: 'run?' , String lf , code ] ]
		on: Error
		do: [ :e | self append: 'error: ' , e messageText , String lf ].
	self append: String lf.
	running := false ] forkNamed: 'yuuki agent'
]
