Class {
	#name : 'YuukiToolTest',
	#superclass : 'TestCase',
	#category : 'Yuuki-Tests'
}

{ #category : 'tests' }
YuukiToolTest >> testErrorsAreText [
	| result |
	result := YuukiTool run: 'Transcript show: ''before''. 1 / 0' timeout: 5.
	self assert: (result includesSubstring: 'before').
	self assert: (result includesSubstring: 'error: ')
]

{ #category : 'tests' }
YuukiToolTest >> testRunCapturesOutputAndValue [
	| result |
	result := YuukiTool run: 'Transcript show: ''hello''. 3 + 4' timeout: 5.
	self assert: (result includesSubstring: 'hello').
	self assert: (result includesSubstring: '=> 7')
]

{ #category : 'tests' }
YuukiToolTest >> testTimeout [
	| result |
	result := YuukiTool run: '[ true ] whileTrue: [ 1 + 1 ]' timeout: 1.
	self assert: (result includesSubstring: 'timed out')
]

{ #category : 'tests' }
YuukiToolTest >> testTruncate [
	| long |
	long := String new: 100000 withAll: $x.
	self assert: (YuukiTool truncate: long) size <= YuukiTool maxResultChars.
	self assert: ((YuukiTool truncate: long) includesSubstring: 'truncated').
	self assert: (YuukiTool truncate: 'short') equals: 'short'
]
