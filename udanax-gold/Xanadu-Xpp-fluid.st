Object subclass: #Emulsion
	instanceVariableNames: '
		myFluids {UNKNOWN}
		myFluidsUsed {UNKNOWN}'
	classVariableNames: 'TheImageEmulsion {UNKNOWN} '
	poolDictionaries: ''
	category: 'Xanadu-Xpp-fluid'!

(Emulsion getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!

!Emulsion methodsFor: 'accessing'!

{void star} fetchNewRawSpace: size {#size.U.t var}

	self subclassResponsibility!

{void star} fetchOldRawSpace

	self subclassResponsibility!

{Array} fluidsSpace
	| result {Array} |
	result _ self fetchOldRawSpace.
	result ~~ NULL ifTrue:
		[ ^ result ].
	result _ self fetchNewRawSpace: myFluids size.
	myFluidsUsed _ true.
	myFluids do: [ :fluid | fluid init ].
	^ result.! !

!Emulsion methodsFor: 'create'!

create
	super create.
	myFluids _ OrderedCollection new.
	myFluidsUsed _ false.! !

!Emulsion methodsFor: 'finalization'!

destructAll
	myFluids do: [ :f | f fluidSet: nil ]! !

!Emulsion methodsFor: 'printing'!

printOn: aStream
	aStream << self class name << '(' << myFluids << ')'! !

!Emulsion methodsFor: 'special'!

{Integer} addFluid: fluid {FluidVar}
	"add a new fluid to the emulsion, and return its index"
	myFluidsUsed not assert: 'Fluids may not be added to emulsion after init.'.
	myFluids add: fluid.
	^ myFluids size!

reset
	myFluids ~~ nil ifTrue: [myFluids do: [ :f | f goAway ]].
	myFluids _ OrderedCollection new.
	myFluidsUsed _ false.! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Emulsion class
	instanceVariableNames: ''!

(Emulsion getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!

!Emulsion class methodsFor: 'accessing'!

imageEmulsion

	^ TheImageEmulsion! !

!Emulsion class methodsFor: 'global: metamorphic creation'!

{Emulsion} globalEmulsion

	^ GlobalEmulsion make! !

!Emulsion class methodsFor: 'cleanup'!

cleanupGarbage
	Emulsion subclassesDo: [:c | c allInstances do: [:e | e ~~ TheImageEmulsion ifTrue: [e reset]]]! !

!Emulsion class methodsFor: 'initialize'!

initialize
	"Emulsion initialize"

	self cleanupGarbage.
	TheImageEmulsion ~~ nil ifTrue:
		[TheImageEmulsion reset.
		TheImageEmulsion _ nil].
	Smalltalk garbageCollect.
	TheImageEmulsion _ GlobalEmulsion new create.
	self initImageEmulsions: Object!

initImageEmulsions: class

	class isMeta ifTrue: [^self].
	(class class includesSelector: #initImageEmulsion)
		ifTrue: [class initImageEmulsion].
	class subclassesDo: [:cl | self initImageEmulsions: cl]! !


Emulsion subclass: #GlobalEmulsion
	instanceVariableNames: 'myFluidsSpace {Array of: Object}'
	classVariableNames: 'TheGlobalEmulsion {Emulsion} '
	poolDictionaries: ''
	category: 'Xanadu-Xpp-fluid'!

(GlobalEmulsion getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!

!GlobalEmulsion methodsFor: 'accessing'!

{void star} fetchNewRawSpace: size {#size.U.t var}

	["cxx: return (myFluidsSpace = (char *) calloc (size, sizeof(char)));"] translateOnly.
	[^myFluidsSpace _ Array new: size] smalltalkOnly!

{void star} fetchOldRawSpace

	^myFluidsSpace! !

!GlobalEmulsion methodsFor: 'create'!

create
	super create.
	myFluidsSpace _ NULL! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

GlobalEmulsion class
	instanceVariableNames: ''!

(GlobalEmulsion getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!

!GlobalEmulsion class methodsFor: 'make'!

make
	(TheGlobalEmulsion == nil or: [TheGlobalEmulsion == NULL]) ifTrue:
		[TheGlobalEmulsion _ self new create].
	^ TheGlobalEmulsion! !

!GlobalEmulsion class methodsFor: 'global: make'!

globalEmulsion
	^ self make! !

!GlobalEmulsion class methodsFor: 'cleanup'!

cleanupGarbage
	TheGlobalEmulsion _ NULL!

exitTimeNonInherited

	TheGlobalEmulsion := NULL.!

linkTimeNonInherited

	TheGlobalEmulsion := NULL.! !

Object subclass: #FluidVar
	instanceVariableNames: '
		myInitBlock {BlockClosure}
		myEmulsion {Emulsion}
		myOffset {Integer}
		myName {Symbol}
		myType {Class}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-fluid'!



!FluidVar methodsFor: 'private: accessing'!

emulsion
	^ myEmulsion! !

!FluidVar methodsFor: 'obsolete'!

fluidVar
	"Return the fluidly bound value."
	^ myEmulsion fluidsSpace at: myOffset! !

!FluidVar methodsFor: 'accessing'!

fluidBind: value during: dynamicBlock
	"Take on new definition during execution of a block, and assure that old value is restored.
	For the benefit of smalltalk only code that may want to use this in an expression, the result of the block evaluation is returned."
	| old oldSpace result |
	old _ self fluidFetch.
	oldSpace _ myEmulsion fetchOldRawSpace.
	self fluidSet: value.
	result _ dynamicBlock
		valueNowOrOnUnwindDo: 
			[self fluidSet: old].
	myEmulsion fetchOldRawSpace == oldSpace assert: 'Emulsion space switched during fluidBind'.
	^ result!

fluidFetch
	"Return the fluidly bound value."
	^ myEmulsion fluidsSpace at: myOffset!

fluidGet
	"Return the fluidly bound value."
	self fluidFetch == NULL ifTrue: [Heaper BLAST: #NULLFluid].
	^self fluidFetch!

fluidSet: newValue
	"self == CurrentPacker ifTrue: [self halt]."
	"assign a new value to the fluid variable"
	myEmulsion fluidsSpace ~~ nil ifTrue: [myEmulsion fluidsSpace at: myOffset put: newValue]!

{void} init
	"called by Emulsion fluidsSpace when a new space is acquired"
	self fluidSet: self initialValue!

{Object} initialValue
	^ myInitBlock ~~ nil ifTrue: [myInitBlock value] ifFalse: [nil].! !

!FluidVar methodsFor: 'create'!

create: initialBlock {BlockClosure} with: emulsion {Emulsion} with: name {Symbol} with: class {Class}
	"This create the first fluid variable within the emulsion"
	super create.
	myInitBlock _ initialBlock.
	emulsion ~~ NULL assert: 'Must have an emulsion'.
	myEmulsion _ emulsion.
	myOffset _ myEmulsion addFluid: self.
	myName _ name.
	myType _ class.! !

!FluidVar methodsFor: 'printing'!

printOn: aStream
	aStream << self getCategory name << '('.
	myEmulsion ~~ nil
	ifTrue:
		[aStream << myEmulsion class << '/' << myName
		<< '{' << myType << '}' << ',  ' << myOffset.
		myOffset ~~ nil
		ifTrue: ["aStream << '->' << (myEmulsion fluidsSpace at: myOffset)"]
		ifFalse: [aStream << '->nil myOffset']]
	ifFalse:
		[aStream << '???????'].
	aStream << ')'! !

!FluidVar methodsFor: 'special'!

goAway
	myEmulsion _ nil.
	myInitBlock _ nil.
	myName ~~ nil ifTrue:
		[Smalltalk safeAt: myName asSymbol put: nil].! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

FluidVar class
	instanceVariableNames: ''!



!FluidVar class methodsFor: 'cleanup'!

cleanup: varName {Symbol}
	"If there is a fluidVar by the given name, set its value to nil"
	| var |
	var _ Smalltalk at: varName asSymbol ifAbsent: [].
	(var isKindOf: FluidVar)
		ifTrue:
			[(var emulsion isKindOf: Emulsion)
				ifTrue: [var fluidSet: nil]
				ifFalse: [Transcript show: varName asString, ' has no emulsion.']]!

{void} cleanupGarbage
	"Reset all fluid variables to their initial states."
	"FluidVar cleanupGarbage."
	
	FluidVar allInstancesDo: [:fluid |
		fluid emulsion ~~ Emulsion imageEmulsion ifTrue: [fluid goAway]]! !

!FluidVar class methodsFor: 'make'!

make: varName {String} 
	with: initialValueBlock {BlockClosure | nil} 
	with: emulsion {Emulsion}
	with: class {Class}
	
	(Smalltalk at: varName asSymbol ifAbsent: []) isBehavior 
		ifTrue: [self error: varName, 'cannot be used as a fluid because it conflicts with a class name.'].
	Smalltalk safeAt: varName asSymbol put:
		(self create: initialValueBlock with: emulsion with: varName asSymbol with: class)! !

!FluidVar class methodsFor: 'smalltalk: passe'!

make: varName {String}
	
	self passe!

make: varName {String} with: initialValue {Heaper | NULL}
	
	self passe! !

FluidVar subclass: #FluidPromiseVar
	instanceVariableNames: 'myActualFluid {Symbol}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-fluid'!
FluidPromiseVar comment:
'We have to use the name of the actual fluid variable since the variable itself may not be
initialized when its corresponding promise fluid variable is initialized.  Since this happens during
static init time, the order cannot be controlled.'!



!FluidPromiseVar methodsFor: 'create'!

create: initialBlock {BlockClosure} 
	with: emulsion {Emulsion} 
	with: name {Symbol} 
	with: class {Class}
	with: actual {FluidVar}
	
	super create: initialBlock with: emulsion with: name with: class.
	actual == nil ifTrue: [self halt].
	myActualFluid := actual! !

!FluidPromiseVar methodsFor: 'accessing'!

fluidSet: newValue
	
	super fluidSet: newValue.
	(newValue == NULL or: [newValue isBroken]) ifTrue:
		[(Smalltalk at: myActualFluid) fluidSet: NULL]
	ifFalse:
		[(Smalltalk at: myActualFluid) fluidSet: newValue actualThing]! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

FluidPromiseVar class
	instanceVariableNames: ''!



!FluidPromiseVar class methodsFor: 'make'!

make: varName {String} 
	with: initialValueBlock {BlockClosure | nil} 
	with: emulsion {Emulsion}
	with: class {Class}
	with: actual {FluidVar}
	
	(Smalltalk at: varName asSymbol ifAbsent: []) isBehavior 
		ifTrue: [self error: varName, 'cannot be used as a fluid because it conflicts with a class name.'].
	Smalltalk safeAt: varName asSymbol put:
		(self create: initialValueBlock with: emulsion with: varName asSymbol with: class with: actual)! !


Emulsion initialize!
