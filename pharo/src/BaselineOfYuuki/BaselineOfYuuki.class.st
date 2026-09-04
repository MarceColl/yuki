Class {
	#name : 'BaselineOfYuuki',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfYuuki'
}

{ #category : 'baselines' }
BaselineOfYuuki >> baseline: spec [
	<baseline>
	spec for: #common do: [
		spec package: 'Yuuki'.
		spec package: 'Yuuki-Tests' with: [ spec requires: #('Yuuki') ].
		spec package: 'Yuuki-UI' with: [ spec requires: #('Yuuki') ].
		spec group: 'default' with: #('Yuuki' 'Yuuki-Tests' 'Yuuki-UI') ]
]
