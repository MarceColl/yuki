"
The one tool: evaluate Smalltalk in the image, Transcript captured, value printed.
"
Class {
	#name : 'YuukiTool',
	#superclass : 'Object',
	#category : 'Yuuki'
}

{ #category : 'listing' }
YuukiTool class >> definitions [
	"Every class and method the agent has defined in the package Yuuki-User."
	| package out |
	package := self userPackage.
	package ifNil: [ ^ 'nothing defined yet' ].
	out := WriteStream on: String new.
	(package definedClasses asSortedCollection: [ :a :b | a name <= b name ]) do: [ :class |
		out nextPutAll: class name; nextPutAll: ' (' , class superclass name , ')'; lf.
		((class methods , class class methods) asSortedCollection: [ :a :b | a selector <= b selector ]) do: [ :method |
			out nextPutAll: '  ';
				nextPutAll: (method methodClass isMeta ifTrue: [ 'class ' ] ifFalse: [ '' ]);
				nextPutAll: method selector;
				lf ] ].
	^ out contents
]

{ #category : 'configuration' }
YuukiTool class >> maxResultChars [
	^ 65536
]

{ #category : 'running' }
YuukiTool class >> run: code timeout: seconds [
	"Evaluate CODE, answering captured Transcript output, the value printed after =>, or the error."
	| out saved capture |
	out := WriteStream on: String new.
	saved := Smalltalk globals at: #Transcript.
	capture := ThreadSafeTranscript new.
	[ Smalltalk globals at: #Transcript put: capture.
	[ | value |
		value := [ Smalltalk compiler evaluate: code ]
			valueWithin: seconds seconds
			onTimeout: [ Error signal: 'timed out after ' , seconds printString , ' s' ].
		out nextPutAll: (capture instVarNamed: 'stream') contents.
		out nextPutAll: '=> ' , value printString; lf ]
		on: Error
		do: [ :e |
			out nextPutAll: (capture instVarNamed: 'stream') contents.
			out nextPutAll: 'error: ' , e messageText; lf ] ]
		ensure: [ Smalltalk globals at: #Transcript put: saved ].
	^ self truncate: out contents
]

{ #category : 'running' }
YuukiTool class >> truncate: text [
	| marker |
	text size <= self maxResultChars ifTrue: [ ^ text ].
	marker := '... [truncated: ' , text size printString , ' chars, cap ' , self maxResultChars printString , ']'.
	^ (text first: (self maxResultChars - marker size - 1 max: 0)) , String lf , marker
]

{ #category : 'listing' }
YuukiTool class >> userPackage [
	^ self packageOrganizer packageNamed: 'Yuuki-User' ifAbsent: [ nil ]
]
