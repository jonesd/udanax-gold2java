Heaper subclass: #XnExecutor
	instanceVariableNames: ''
	classVariableNames: 'TheNoopExecutor {XnExecutor} '
	poolDictionaries: ''
	category: 'Xanadu-wparray'!

(XnExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!

!XnExecutor methodsFor: 'invoking'!

{void} execute: estateIndex {Int32}
	"superclass implements a noop"! !

!XnExecutor methodsFor: 'generated:'!

actualHashForEqual ^self asOop!

isEqual: other ^self == other! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

XnExecutor class
	instanceVariableNames: ''!

(XnExecutor getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!

!XnExecutor class methodsFor: 'smalltalk: init'!

linkTimeNonInherited
	TheNoopExecutor := NULL!

TimeNonInherited
	TheNoopExecutor := XnExecutor create! !

!XnExecutor class methodsFor: 'pseudoconstructor'!

{XnExecutor} noopExecutor
	^ TheNoopExecutor! !