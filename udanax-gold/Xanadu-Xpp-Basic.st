InstructionClient subclass: #ExtractMethodConstant
	instanceVariableNames: 'myConstant {UNKNOWN}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Basic'!



!ExtractMethodConstant methodsFor: 'private: private'!

create
	super create.
	myConstant _ nil! !

!ExtractMethodConstant methodsFor: 'accessing'!

constant 
	^myConstant! !

!ExtractMethodConstant methodsFor: 'opcodes-data movement'!

pushConstant: value
	myConstant _ value! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ExtractMethodConstant class
	instanceVariableNames: ''!



!ExtractMethodConstant class methodsFor: 'operations'!

from: method {CompiledMethod}
	| inStream end extractor |
	inStream _ InstructionStream on: method.
	end _ method bytes size.
	extractor _ self create.
	[extractor constant == nil and: [inStream pc <= end]]
		whileTrue: [inStream interpretNextInstructionFor: extractor].
	^extractor constant! !

Object subclass: #StaticFunctionPointer
	instanceVariableNames: '
		myClass {Class}
		mySelector {Symbol}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Basic'!
StaticFunctionPointer comment:
'An object that represents in Smalltalk the equivalent of a pointer to a class static function in C++. When it gets the invokeFunction:with:... message, it sends mySelector to the class, with the arguments of the invokeFunction message. This does the same thing that the translated code would do if this were in fact a function pointer.'!



!StaticFunctionPointer methodsFor: 'create'!

create: class {Class} with: selector {Selector}
	myClass := class.
	mySelector := selector.! !

!StaticFunctionPointer methodsFor: 'invoking'!

invokeFunction

	^myClass perform: mySelector!

invokeFunction: arg1

	^myClass perform: mySelector
		with: arg1!

invokeFunction: arg1 with: arg2

	^myClass perform: mySelector
		with: arg1
		with: arg2!

invokeFunction: arg1 with: arg2 with: arg3

	^myClass perform: mySelector
		with: arg1
		with: arg2
		with: arg3!

invokeFunction: arg1 with: arg2 with: arg3 with: arg4

	^myClass perform: mySelector
		with: arg1
		with: arg2
		with: arg3
		with: arg4!

invokeFunction: arg1 with: arg2 with: arg3 with: arg4 with: arg5

	| args |
	args := Array new: 5.
	args basicAt: 1 put: arg1.
	args basicAt: 2 put: arg2.
	args basicAt: 3 put: arg3.
	args basicAt: 4 put: arg4.
	args basicAt: 5 put: arg5.
	^myClass perform: mySelector
		withArguments: args!

invokeFunction: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6

	| args |
	args := Array new: 6.
	args basicAt: 1 put: arg1.
	args basicAt: 2 put: arg2.
	args basicAt: 3 put: arg3.
	args basicAt: 4 put: arg4.
	args basicAt: 5 put: arg5.
	args basicAt: 6 put: arg6.
	^myClass perform: mySelector
		withArguments: args! !

!StaticFunctionPointer methodsFor: 'printing'!

printOn: oo
	oo << myClass name << '::' << mySelector! !

!StaticFunctionPointer methodsFor: 'accessing'!

selector
	^mySelector!

staticClass
	^myClass! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

StaticFunctionPointer class
	instanceVariableNames: ''!



!StaticFunctionPointer class methodsFor: 'creation'!

make: class {Class} with: selector {Symbol}
	"You should only call this from Smalltalk code. For translatable code, use
		SomeClass pointerToStaticMember: #selector:with:"
	^self create: class with: selector! !

Object subclass: #DeletedHeaper
	instanceVariableNames: 'myOldHeaper {Heaper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Basic'!



!DeletedHeaper methodsFor: 'getcategory'!

getCategory
	^ Heaper!

isKindOf: aClass
	^ aClass == Heaper! !

!DeletedHeaper methodsFor: 'creation'!

create
	"in preparation for a smalltalk become"
	myOldHeaper _ self!

create: a with: b
	self halt.! !

!DeletedHeaper methodsFor: 'destroy'!

{void NOLOCK} destroy! !

!DeletedHeaper methodsFor: 'inquiry'! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

DeletedHeaper class
	instanceVariableNames: ''!



!DeletedHeaper class methodsFor: 'safe unlocking'!

{void} unlockFunctionAvoidingDestroy: shep {Abraham} 
	"Since all Abrahm methods are nested inside a lock - operate - 
	unlock sequence, destroy can't deallocate. Therefore, the unlock 
	operation tests whether the shepherd has been destructed (by 
	checking its category) and deallocates if so."

	shep destroy! !

Object subclass: #Heaper
	instanceVariableNames: ''
	classVariableNames: '
		AllBlasts {Heaper smalltalk} 
		BecomeMap {Heaper smalltalk} 
		GarbageCount {Heaper smalltalk} 
		InGC {Heaper smalltalk} 
		InitializedClasses {Set smalltalk} 
		InitializingClasses {Set smalltalk} 
		LastMemory {Heaper smalltalk} 
		NextClientRequestNumber {IntegerVar} 
		NotOneElementSignal {Heaper smalltalk} 
		PackageTable {Heaper smalltalk} 
		PromiseNameTable {IdentityDictionary} 
		StringHashSBoxes {Heaper smalltalk} '
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Basic'!
Heaper comment:
'This is the base heap allocated class in X++. '!

(Heaper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!

!Heaper methodsFor: 'anachronisms'!

notWorking
	self error: 'this method does not work...'!

{void} passe
	self error: 'this routine is just passe...'!

unimplemented

	Heaper BLAST: #'NOT_YET_IMPLEMENTED'! !

!Heaper methodsFor: 'create/destroy'!

{void NOLOCK} destroy
	self destruct.
	self destructor.
	self delete!

{void NOLOCK} destruct
	"Classes should implement this message rather than a destructor. We use this so 
	the destruction behavior implemented in abstract superclasses can access the 
	vtable of the concrete run-time type in C++. Using a message makes C++ 
	parallel the Smalltalk semantics for delete.  Destroy will actually call the destructor."! !

!Heaper methodsFor: 'xlate hack'!

arrow: aSymbol
	"This is for references to struct fields.  
		foo arrow: #field  
		translates to
		foo->field."
	self error: 'Not implemented in smalltalk; C++ only operation!!'.
	
	self arrow: #field!

deref
	"This is so that PrimArray arguments can be used identically in smalltalk and X++"
	"Given UInt8Array * array,   array at: i ==> array[i] which is not right."
	"array deref at: i ==> (*(array))[i] which has the desired behavior."
	^ self! !

!Heaper methodsFor: 'stubble'!

{void} create.Rcvr: rcvr {Rcvr}!

{BooleanVar} isByProxy
	^false!

{void} sendProxyTo: trans {Xmtr}
	
	self stubbleForSubclassResponsibility!

{void} sendSelfTo: trans {Xmtr}
	"do nothing"! !

!Heaper methodsFor: 'converting'!

{Heaper} convert: type {Category}
	"do a type conversion and a cast"
	^Converter CONVERT: type with: self! !

!Heaper methodsFor: 'printing'!

{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(????)'.! !

!Heaper methodsFor: 'testing'!

{UInt32} actualHashForEqual
	^ self asOop!

{UInt32} hashForEqual
	^ self actualHashForEqual!

{BooleanVar} isEqual: other {Heaper}
	"Return true if the two objects are equal."
	self subclassResponsibility! !

!Heaper methodsFor: 'accessing'!

{Category} getCategory
	^ self class! !

!Heaper methodsFor: 'twiddles'!

findTail: visited {IdentitySet} into: result {IdentitySet}
	"find a non-Heaper object that refers to this one"
	| owner |
	visited add: self.
	owner _ self firstOwner.
	[owner ~~ nil] whileTrue:
		[(visited includes: owner) ifFalse:
			[visited add: owner.
			(owner isKindOf: Heaper)
				ifTrue: [owner findTail: visited into: result]
				ifFalse: [((owner isKindOf: Collection)
						or: [(owner isKindOf: InstructionClient)
						or: [owner isKindOf: RootHandle]])
					ifFalse: [Transcript show: owner printString; cr.
						result add: owner]]].
		owner _ self ownerAfter: owner].
	^result! !

!Heaper methodsFor: 'smalltalk: gc'!

create!

delete	
	self nilFields.!

{void} destructor
	"No other class should implement this.  This will eventually become
	 deleted objects in Smalltalk to Heaper."
	
	self become: DeletedHeaper create!

isUnlocked
	^true!

{void} markChildren: count {IntegerVar}
	"This ia a generic method to mark all of the pointers coming
	 out of an object.  Subclasses with lots of IntegerVars or with 
	 wimpy pointers should reimplement this."

	| class |
	class _ self class.
	1 to: class instSize do: [:i | (self instVarAt: i) markInstances: count]!

{IntegerVar} markCount
	"Return the count of the last garbage collection i which this object was marked."
	
	^-1!

{void} markInstances: count {IntegerVar} 
	"The count is so we don't need to clear the marks."! !

!Heaper methodsFor: 'packages'!

{void} addPackage: package {Package}
	"add a package to the list"

	(PackageTable at: self ifAbsent: [PackageTable at: self put: (OrderedCollection with: package).  ^self]) add: package!

{Package} fetchPackage: cat {PackageCategory}
	"retrieve a package attached to this object"
	
	| packages {OrderedCollection} |
	packages _ PackageTable at: self ifAbsent: [^NULL].
	packages do: [ :p | (p packagingCategory isEqualOrSubclassOf: cat) ifTrue: [^p]].
	^NULL!

{Package} getOrMakePackage: cat {PackageCategory}
	"retrieve a package attached to this object"
	| result {Package} |
	result _ self fetchPackage: cat.
	result ~~ NULL ifTrue:
		[^result].
	self packagingCategory withAllSuperclasses do: [ :parent {Category} |
		parent packageClasses do: [ :pCat {PackageCategory} |
			(pCat isEqualOrSubclassOf: cat) ifTrue:
				[result _ cat makePackage: self.
				self addPackage: result.
				^result]]].
	Heaper BLAST: #GetFailed!

{Package} pack: packageCat {PackageCategory}
	"this gets translated into a macro PACK(PackageClassName,contents)"
	^self getOrMakePackage: packageCat!

{Category} packagingCategory
	"the category object to use for packaging purposes"
	^self class! !

!Heaper methodsFor: 'smalltalk: passe'!

foo.IntegerVar: x

	self junk! !

!Heaper methodsFor: 'exceptions: exceptions'!

blast: problemName {Symbol}
	"raise an exception"
	
	(Heaper signal: problemName) raise! !

!Heaper methodsFor: 'promise messages'!

{Heaper CLIENT login} cast.IntegerVar: catNum {IntegerVar}
	"Return the object if it is of the appropriate type."
	"Note that cast and isKindOf could be implemented in terms of each other is there were operations to commute between booleans and broken promises."
	
	Dean shouldImplement!

{BooleanVar CLIENT login} equals: other {Heaper}
	"Return true if the two objects are equal."
	^self isEqual: other!

{UInt32 CLIENT login} hash
	^self hashForEqual!

{BooleanVar CLIENT login} isKindOf.IntegerVar: catNum {IntegerVar}
	"The server message to test the run-time type of a promise."
	
	Dean shouldImplement! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Heaper class
	instanceVariableNames: 'myTokens {Dictionary smalltalk} mySelectors {Dictionary smalltalk} myReturn {Dictionary smalltalk} myArguments {Dictionary smalltalk} myPreorderNumber {IntegerVar} myPreorderMax {IntegerVar} myPackageClasses {OrderedCollection smalltalk of: Category} '!

(Heaper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!

!Heaper class methodsFor: 'automatic generation'!

{void} addMethodAttribute: attr {Symbol} to: src {String} in: md {MethodDictionary} of: cat {String}
	| returnDecl {String} firstSp {IntegerVar} insertPt {IntegerVar} newSrc {String} sel {String} |
	returnDecl _ src copyUpTo: $}.
	firstSp _ returnDecl indexOf: $ .
	firstSp == 0 
		ifTrue: [insertPt _ returnDecl size + 1]
		ifFalse: [insertPt _ firstSp].
	newSrc _ src copyReplaceFrom: insertPt to: insertPt - 1 with: attr.
	sel _ (self parserClass new) parseSelector: src.
	ChangeLog log: #accept selector: sel in: self changes: nil.
	(md at: sel) putSource: newSrc class: self category: cat inFile: 2.!

{void} compileGeneratedClassMethod: text {String}
	self class compile: text
		classified: self generatedCategory
		notifying: nil.!

{void} compileGeneratedMethod: text {String}
	self compile: text
		classified: self generatedCategory
		notifying: nil.!

convertCopyDeclarations
	"This converts all copy classes to the new representation.  Rather than copy declaration for instance variables, the classes will have the COPY attribute, and instance variables that should not be copied have the nocopy attribute."
	
	| cxx |
	cxx _ self fetchCxxClassDescription.
	cxx == nil ifTrue: [^self].
	(cxx includesAttribute: #COPY) ifTrue:
		[self warn: self name, ' already has COPY attribute.'. ^self].
	ChangeLog suppressLoggingIn:
	[((cxx instVarTypeString findString: 'copy' startingAt: 1) ~~ 0
		or: [self class includesSelector: #copyClass])
		ifTrue:
			[cxx convertCopyDeclarations.
			(self class includesSelector: #copyClass) ifTrue: 
				[self class removeSelector: #copyClass.
				cxx addAttribute: #COPY]]]!

convertDeferredDeclarations
	"This converts all deferred classes to the new representation.  Rather than deferred: declaration for protocols, the classes will have the DEFERRED attribute."
	

	| flag  {BooleanVar} cxx |
	cxx _ self fetchCxxClassDescription.
	cxx == nil ifTrue: [^self].
	flag _ false.
	self organization categories do: [:cat|
	(cat findString: 'deferred' startingAt: 1) ~= 0
		ifTrue: [flag _ true]].
	flag ifFalse:
		[ | special |
		special _ Smalltalk specialSelectorIndexOf: #subclassResponsibility.
		methodDict do: [:method | 
			((method refersToLiteral: #subclassResponsibility)
				or: [special > 0 and: [method sendsSpecialSelector: special]])
			  ifTrue: [flag _ true]]].
	ChangeLog suppressLoggingIn: [flag
		ifTrue: [cxx addAttribute: #DEFERRED]
		ifFalse: [cxx addAttribute: #CONCRETE]]!

convertProxyDeclarations
	"This converts all Proxy classes to the new representation.  Rather than a proxy: declaration for protocols, the classes will have the BY.PROXY attribute.  Methods which were in proxy: protocols should have PROXY attributes.  In addition methods which override PROXY methods will also be declared PROXY."

	| cxx {CxxClassDescription} isProxy {BooleanVar}  hasNewlyDeclaredMethods {BooleanVar} |
	cxx _ self fetchCxxClassDescription.
	cxx == nil ifTrue: [^self].
	isProxy _ false.
	hasNewlyDeclaredMethods _ false.
	self organization categories do:
		[:cat |  (self protocol: cat hasAttribute: #proxy)
			ifTrue:
				[(self  organization listAtCategoryNamed: cat) do:
				[ :selector |
				(self selector: selector hasAttribute: #PROXY) ifFalse:
					[ | source {String} |
					source _ (methodDict at: selector) getSource.
					isProxy _ true.
					self addMethodAttribute: ' PROXY' to: source in: methodDict of: cat]]]
			ifFalse:
				[((self protocol: cat hasAttribute: #generated) or: 
				[self protocol: cat hasAttribute: #smalltalk]) ifFalse:
					[(self  organization listAtCategoryNamed: cat) do:
						[ :sel |
						((self allSuperclasses
									detect: [:sClass |
									(sClass selectors includes: sel)
										ifTrue: [sClass selector: sel hasAttribute: #PROXY]
										ifFalse: [false]]
									ifNone: [nil]) isNil not and: 
										[(self selector: sel hasAttribute: #PROXY) not]) 
								ifTrue:
									[ | src {String} |
									src _ (methodDict at: sel) getSource.
									hasNewlyDeclaredMethods _ true.
									self addMethodAttribute: ' PROXY' to: src in: methodDict of: cat]]]]].
	isProxy ifTrue: [cxx addAttribute: #BY.PROXY].
	hasNewlyDeclaredMethods ifTrue: [Transcript cr; show: self name, ' has Changed'].!

convertSubclassCopyDeclarations
	"This converts all copy classes to the new representation.  Rather than copy declaration for instance variables, the classes will have the COPY attribute, and instance variables that should not be copied have the nocopy attribute."
	
	"Heaper convertSubclassCopyDeclarations"
	
	self withAllSubclasses do: [:cl | cl convertCopyDeclarations]!

convertSubclassDeferredDeclarations
	"This converts all deferred classes to the new representation.  Rather than deferred declaration for protocols, the classes will have the DEFERRED attribute."
	
	"Heaper convertSubclassDeferredDeclarations"
	
	self withAllSubclasses do: [:cl | cl convertDeferredDeclarations]!

convertSubclassProxyDeclarations
	"This converts all deferred classes to the new representation.  Rather than proxy declaration for protocols, the classes will have the By.Proxy attribute."
	
	"Heaper convertSubclassProxyDeclarations"
	
	self withAllSubclasses do: [:cl | cl convertProxyDeclarations]!

generatedCategory
	^#'generated:'!

isGenerated
	"Automatically generated classes should implement this as true."
	
	^false!

removeGeneratedCode
	ChangeLog suppressLoggingIn:
		[self isGenerated
			ifTrue: [self removeFromSystem]
			ifFalse: [self removeCategory: #generated:.
					self class removeCategory: #generated:]]!

removeSubclassGeneratedCode
	"remove subclass methods on me and all subclasses"
	"Heaper removeSubclassGeneratedCode"
	self cleanPromiseClasses.
	self withAllSubclasses do: [ :class |
		class removeGeneratedCode].
	(self == Heaper or: [self == RequestHandler]) ifTrue:
		[ChangeLog suppressLoggingIn:
			[RPCSpecialist removeCategory: #'translate: generated'.
			Request class removeCategory: #'extern: evaluate'.
			PromiseManager class removeCategory: #'translate: generated'.
			RequestHandler class removeCategory: #'translate: generated']]! !

!Heaper class methodsFor: 'stubble PSEUDO_COPY'!

compileSendSelfToSendHook
	"for pseudocopy. compile a method to send self to a xmtr, but only through SEND.HOOK"
	| source {String} |
	source _ String streamContents: [ :stream |
		stream nextPutAll: '{void} sendSelfTo: xmtr {Xmtr}'; crtab.
		self organization categories do:
			[:cat |
			((cat testPrefix: 'sender') or: [cat testPrefix: 'hooks'])
				ifTrue: [(self organization listAtCategoryNamed: cat) do:
					[:sel |
					(self selector: sel hasAttribute: #SEND.HOOK)
						ifTrue: [ stream crtab << 'self ' << sel << ' xmtr.']]]]].
	self compileGeneratedMethod: source.!

make.Rcvr: rcvr
	self unimplemented! !

!Heaper class methodsFor: 'stubble COPY'!

compileCreateFromRcvr: ivars {OrderedCollection of: Association}
	"compile a method to create self from a receiver"
	| source {String} |
	source _ String streamContents: [ :stream |
		stream nextPutAll: 'create.Rcvr: receiver {Rcvr}'; cr; tab;
			nextPutAll: 'super create.Rcvr: receiver.'.
		ivars do: [ :variable | 
			(variable value hasAttribute: #NOCOPY) ifFalse: 
				[stream crtab << variable key << ' _ receiver '.
				stream << 'receive' << (self abstractTypeFor: variable value baseType asSymbol) << $.]].
		self organization categories do: [:cat |
			((self protocol: cat hasAttribute: #receiver) 
			 or: [self protocol: cat hasAttribute: #hooks]) ifTrue:
				[(self organization listAtCategoryNamed: cat)
					do: [:sel | (self selector: sel hasAttribute: #'RECEIVE.HOOK')
						ifTrue: [stream crtab << 'self ' << sel << ' receiver.']]]]].
	self compileGeneratedMethod: source.!

compileSendSelfTo: ivars {OrderedCollection of: Association}
	"compile a method to send self to a xmtr"
	| source {String} |
	source _ String streamContents: [ :stream |
		stream nextPutAll: '{void} sendSelfTo: xmtr {Xmtr}'; crtab.
		stream nextPutAll: 'super sendSelfTo: xmtr.'.
		ivars do: [ :variable |
			(variable value hasAttribute: #NOCOPY) ifFalse: 
				[stream crtab << 'xmtr send' << (self abstractTypeFor: variable value baseType asSymbol) << ': ' << variable key << $.]].
		self organization categories do:
			[:cat |
			((self protocol: cat hasAttribute: #sender) 
			 or: [self protocol: cat hasAttribute: #hooks])
				ifTrue: [(self organization listAtCategoryNamed: cat) do:
					[:sel |
					(self selector: sel hasAttribute: #SEND.HOOK)
						ifTrue: [ stream crtab << 'self ' << sel << ' xmtr.']]]]].
	self compileGeneratedMethod: source.!

findSenderAndReceiverMethods
	Cursor execute showWhile: 
			[| coll |
			coll _ OrderedCollection new.
			Heaper withAllSubclasses do: 
				[:cl | cl organization categories do: 
					[:cat | 
					((self protocol: cat hasAttribute: #sender)
						or: [self protocol: cat hasAttribute: #receiver])
						ifTrue: [(cl organization listAtCategoryNamed: cat)
								do: [:sel {Symbol} | coll add: cl name , ' ' , sel]]]].
			BrowserView
				openListBrowserOn: coll
				label: 'Sender and Receiver methods'
				initialSelection: nil]! !

!Heaper class methodsFor: 'stubble'!

compileClientSubclasses
	"Heaper compileClientSubclasses"
	self cachePromiseNameTableIn: 
	[self withAllSubclasses do: [:class | 
		(class hasAttribute: #ON.CLIENT)
			ifTrue: [ChangeLog suppressLoggingIn: [class compilePromise]]]]!

compileStubbleMethods
	"compile makeProxy, sendSelfTo and create.Xcvr:"
	"Rational compileStubbleMethods"
	
	| cxx {CxxClassDescription} copy {Collection of: Association} |
	cxx _ self fetchCxxClassDescription.
	cxx == nil ifTrue: [^self].
	ChangeLog suppressLoggingIn:
		[copy _ (cxx parsedInstVarTypes
			collect: [ :node {ParameterNode} |
				node name -> (TypeDescription fromParse: node type with: self)])
			select: [ :pair {Association of: Symbol and: TypeDescription} |
				pair value isNoCopy not].
		(cxx includesAttribute: #COPY) ifTrue:
			[self compileSendSelfTo: copy.
			self compileCreateFromRcvr: copy].
		"(cxx includesAttribute: #ON.CLIENT) ifTrue:
			[self compilePromise]."
		(cxx includesAttribute: #PSEUDO.COPY) ifTrue:
			[self compileSendSelfToSendHook].
		(cxx includesAttribute: #EQ) ifTrue:
			[self compileEQ].
		((cxx includesAttribute: #HOOK) or: [cxx includesAttribute: #PACKAGE.HOOK]) ifTrue:
			[self compileHook]]!

compileSubclassStubbleMethods
	"compile subclass methods on me and all subclasses"
	"Heaper compileSubclassStubbleMethods"
	self cachePromiseNameTableIn:
	[self withAllSubclasses do: [ :class | 
		class compileStubbleMethods]]!

removeStubbleMethods
	ChangeLog suppressLoggingIn:
		[(self superclass name , 'Proxy' = self name)
			ifTrue: [self removeFromSystem]
			ifFalse: 
				[self removeCategory: #'smalltalk: stubble'.
				self removeCategory: #'smalltalk stubble'.]]!

removeSubclassStubbleMethods
	"remove subclass methods on me and all subclasses"
	"Heaper removeSubclassStubbleMethods"
	self withAllSubclasses do: [ :class |
		class removeStubbleMethods].
	self == Heaper ifTrue:
		[ChangeLog suppressLoggingIn:
			[RPCSpecialist removeCategory: #'translate: generated']]! !

!Heaper class methodsFor: 'private: stubble accessing'!

{OrderedCollection of: Symbol} argumentTypesFor: selector {Selector}
	"the argument types for the given message"
	^(myArguments ~~ nil and: [myArguments includesKey: selector])
		ifTrue: [myArguments at: selector]
		ifFalse: [superclass argumentTypesFor: selector]!

{Symbol} computeMangle: tree {ParseNode}
	"compute the C++ mangled name for a function"
	#delete == tree selector ifTrue: [^#delete].
	^(String streamContents: [ :stream |
		stream nextPutAll: (HxxPrintStream mapSelector: tree selector);
"			nextPutAll: '__';"
			print: (HxxPrintStream mapVarName: self name) size;
			nextPutAll: (HxxPrintStream mapVarName: self name);
			nextPut: $F.
		tree block arguments isEmpty ifTrue:
			[stream nextPut: $v]
		ifFalse:
			[tree block arguments do: [ :argument {ParseNode} |
				stream nextPutAll: (TypeDescription fromParse: argument type with: self) mangle]]])
	asSymbol!

{Boolean} definesProxyMethods
	^(self getCxxClassDescription includesAttribute: #BY.PROXY)
		and: [nil ~~ (self selectors
				detect: [:sel {Symbol} |
						self selector: sel hasAttribute: #PROXY]
				ifNone: [nil])]!

{Boolean} hasProxyMethods
	^self definesProxyMethods
		or: [self ~~ Heaper and: [superclass hasProxyMethods]]!

{Symbol} mangle: selector {Symbol}
	"return the C++ mangled function name from the Smalltalk selector"
	^(myTokens ~~ nil and: [myTokens includesKey: selector])
		ifTrue: [myTokens at: selector]
		ifFalse: [superclass mangle: selector]!

{Symbol} returnTypeFor: selector {Selector}
	"the return type for the given message"
	self thingToDo. "either cache this or put a dictionary in a class instance variable"
	^(myReturn ~~ nil and: [myReturn includesKey: selector])
		ifTrue: [myReturn at: selector]
		ifFalse: [superclass returnTypeFor: selector]!

stubbleSelector: selector {Symbol} token: token {Symbol} returns: return {Symbol} arguments: arguments {Array of: Symbol}
	"add stubble information to the class"
	myTokens == nil ifTrue:
		[mySelectors _ IdentityDictionary new.
		myTokens _ IdentityDictionary new.
		myReturn _ IdentityDictionary new.
		myArguments _ IdentityDictionary new].
	mySelectors at: token put: selector.
	myTokens at: selector put: token.
	myReturn at: selector put: return.
	myArguments at: selector put: arguments.!

{Symbol} unmangle: token {Symbol}
	"return the Smalltalk selector from the C++ mangled function name"
	^(mySelectors ~~ nil and: [mySelectors includesKey: token])
		ifTrue: [mySelectors at: token]
		ifFalse: [superclass unmangle: token]!

wipeStubble
	"remove stubble information"
	myTokens _ mySelectors _ myReturn _ myArguments _ nil.! !

!Heaper class methodsFor: 'stubble PROXY'!

compileProxy
	"make a proxy subclass with all message that are in my or superclasses' proxy sections"
	| proxy {Class} |
	proxy _ self compileProxyClass.
	self compileStubMethods: proxy.
	self compileGeneratedMethod: '{BooleanVar} isByProxy ^true'.

	self subclassesDo: [:sc |
		sc compileGeneratedMethod:
'{void} sendProxyTo: xmtr {Xmtr}
	super sendSelfTo: xmtr'].!

{Class} compileProxyClass
	"make a proxy subclass with its general messages and return it"
	| proxy {Class} superCreateString {String} file {CxxTreeAssociation} |
	proxy _ self subclass: ((self name copyUpTo: $.) , 'Proxy') asSymbol
		instanceVariableNames: 'mySpecialist {RPCSpecialist}'
		classVariableNames: ''
		poolDictionaries: ''
		category: self category.
		
	proxy compileGeneratedClassMethod:
'{BooleanVar} isGenerated ^true'.

	file _ CxxSystemOrganization fetchTreeLocForFile: self getCxxClassDescription fetchGroup cxxModule moduleName.
	[file value == #dir or: [file value isNil]] whileFalse:
		[file _ file parent].
	"proxy compileGeneratedClassMethod:
'initTimeNonInherited
	self REQUIRES: Recipe.
	STProxyRecipe create: self with: ''', (file value isNil ifTrue: ['NULL'] ifFalse: [file key]), ''' with: NULL'."

	superCreateString _ (self getCxxClassDescription includesAttribute: #COPY)
							ifTrue: ['super create.Rcvr: rcvr.']
							ifFalse:['super create.'].

	proxy compileGeneratedMethod:
'create: rcvr {Rcvr} with: spec {RPCSpecialist}
	', superCreateString, '
	mySpecialist _ spec.'.

	self compileGeneratedMethod:
'{Category} getProxyCategory
	^', self printString, 'Proxy'.


	^proxy!

{void} compileStubMethod: selector in: proxy token: token return: return arguments: arguments category: cat
	| source {String} |
	source _ (String streamContents: [:oo {Stream} |

		oo << '{' << return << '} '.
		arguments isEmpty
			ifTrue:
				[oo << selector]
			ifFalse: 
				[ | selectorTokens {OrderedCollection of: String} |
				selectorTokens _ OrderedCollection new.
				selector tokensDo: [:tok | selectorTokens add: tok].
				1 to: arguments size do:
					[:argC {IntegerVar} |
					oo << (selectorTokens at: argC) << ': '.
					oo << 'arg' << argC printString.
					oo << ' {' << (arguments at: argC) << '} ']].
		oo crtab.
		(#(void NOWAIT IntegerVar BooleanVar) includes: return) ifFalse:
			[oo << '^('].
		(#(IntegerVar BooleanVar) includes: return) ifTrue: [oo << '^'].
		oo << 'mySpecialist request' << (self abstractTypeFor: return) << ': self '.
		oo << 'with: ''' << token << ''''.
		arguments isEmpty ifFalse: [1 to: arguments size do: 
			[:argC {IntegerVar} |
			oo << ' with' << (self abstractTypeFor: (arguments at: argC)).  "got rid of dot in caller"
			oo << ': arg' << argC printString]].
		(#(void NOWAIT IntegerVar BooleanVar) includes: return) ifFalse: 
			[oo << ') cast: ' << return]]).

	proxy compile: source
		classified: cat
		notifying: nil!

compileStubMethods: proxy {Class} 
	"compile stub methods for all selectors in proxy sections"
	"update return types, argument types, mangling and unmangling information"
	self wipeStubble.
	self withAllSuperclasses reverseDo:
		[ :declaringClass |
		declaringClass selectors do: [ :selector | | source {String} |
			source := declaringClass sourceCodeStringAt: selector.
			(source indexOfSubCollection: 'PROXY' startingAt: 1) ~= 0 ifTrue:
				[ | stub {Symbol} tree {MethodNode} returnType {TypeDescription} token {Symbol}
				   return {Symbol} arguments {Collection of: Symbol} |
				stub := ('generated: ' , (declaringClass organization categoryOfElement: selector)) asSymbol.
				tree := Parser new parse: source class: declaringClass notifying: nil.
				returnType := tree returnTypeDescription.
				(returnType hasAttribute: #PROXY) ifTrue:
					["declaringClass == self
						ifTrue: [token := self computeMangle: tree]
						ifFalse: [token := self mangle: selector]."
					self hack.  "We aren't mangling names."
					token _ selector.
					return := returnType stubbleName.
					arguments := (tree block arguments collect: [ :argument |
						(TypeDescription fromParse: argument type with: self) stubbleName]) asArray.
					self stubbleSelector: selector token: token returns: return arguments: arguments.
					
					self compileStubMethod: selector in: proxy token: token return: return	
						arguments: arguments category: stub.
					self createRequestClass: return arguments: arguments.]]]]! !

!Heaper class methodsFor: 'stubble REQUEST'!

{void} compileRequestCreateMsgIn: requestClass {Class} arguments: args {OrderedCollection of: Symbol}
	| method {String} |

	method _ (args isEmpty
		ifFalse: [(String streamContents: [:oo {Stream} |
			oo << 'create: msgName {char star} with: rcvr {Heaper}'.
			1 to: args size do: [:argC {IntegerVar} |
				oo << ' with: arg' << argC printString << ' ' << (self abstractDeclarationFor: (args at: argC))].
			oo crtab.
			oo << 'super create: msgName with: rcvr'.
			1 to: args size do: [:argC {IntegerVar} |
				oo << '.'; crtab.
				oo << 'myArg' << argC printString << ' _ arg' << argC printString]])]
		ifTrue: [
'create: msgName {char star} with: rcvr {Heaper}
	super create: msgName with: rcvr']).

	requestClass compile: method
		classified: 'creation'
		notifying: nil.!

{void} compileRequestEvaluateMsgIn: requestClass {Class} returning: return {Symbol} arguments: args {OrderedCollection of: Symbol}
	| method {String} function {String} |

	method _ (String streamContents:
		[:oo {Stream} |
		oo << '{Response} evaluate'; crtab.
		(return = #void or: [return = #NOWAIT])
			ifFalse:
				[oo << '| result ' << (self abstractDeclarationFor: return) << ' |'; crtab.
				oo << 'result _ '].
		oo << 'Request evaluate' << (self abstractTypeFor: return) << ': self receiver with: self messageName'.
		args isEmpty
			ifFalse: [1 to: args size do:
				[:argC {IntegerVar} |
				oo << ' with' << (self abstractTypeFor: (args at: argC)) << ': myArg' << argC printString]].
		oo << '.'; crtab.
		return = #void
			ifTrue: [oo << '^Response make']
			ifFalse: [return = #NOWAIT
				ifTrue: [oo << '^Response noWaitResponse']
				ifFalse: [oo << ('^Response ' , (self abstractTypeFor: return) asUncapitalized , 'Response: result')]]]).


	function _ (String streamContents:
		[:oo {Stream} |
		(return = #NOWAIT)
			ifTrue: [oo << '{void}' ]
			ifFalse: [oo << (self abstractDeclarationFor: return)].
		oo << ' evaluate' << (self abstractTypeFor: return) << ': receiver {Heaper} with: msgName {char star}'.
		args isEmpty
			ifFalse: [1 to: args size do:
				[:argC {IntegerVar} |
				oo << ' with' << (self abstractTypeFor: (args at: argC)) << ': arg' << argC printString.
				oo << ' ' << (self abstractDeclarationFor: (args at: argC))]].
		oo crtab.
		(return = #NOWAIT or: [return = #void])
				ifFalse: [oo << '^'].
		 oo << 'receiver perform: msgName asSymbol'.
		args isEmpty
			ifFalse: [1 to: args size do:
				[:argC {IntegerVar} |
				oo << ' with: arg' << argC printString]]]).

	requestClass compile: method
		classified: 'evaluate'
		notifying: nil.

	Request class compile: function
		classified: 'extern: evaluate'
		notifying: nil!

{void} compileRPCSpecialistEvaluateMsgFor: requestName {String} returning: return {Symbol} arguments: args {OrderedCollection of: Symbol}
	| method {String} abstractReturn {String} |

	abstractReturn _ self abstractTypeFor: return.

	method _ (String streamContents:
		[:oo {Stream} |
		oo << (self abstractDeclarationFor: return) << ' request' << abstractReturn << ': proxy {Heaper} with: msg {char star}'.
		args isEmpty
			ifFalse: [1 to: args size do:
				[:argC {IntegerVar} |
				oo << ' with' << (self abstractTypeFor: (args at: argC)) << ': arg' << argC printString << ' '.
				oo << (self abstractDeclarationFor: (args at: argC))]].
		oo << '
	| req {Request} |
	req _ ' << requestName << '
			create: msg
			with: proxy'.
		args isEmpty
			ifFalse: [1 to: args size do:
				[:argC {IntegerVar} |
				oo crtab; tab; tab.
				oo << ' with: arg' << argC printString]].
		oo << '.
	self sendRequest: req.'.
		return = #NOWAIT ifFalse: [
			oo crtab.
			return = #void
				ifTrue: [ oo << 'self waitForResponse']
				ifFalse: [oo << '^(self waitForResponse cast: ' << abstractReturn << 'Response) value']]]).

	RPCSpecialist compile: method
		classified: 'translate: generated' asSymbol
		notifying: nil.!

{void} createRequestClass: return {Symbol} arguments: arguments {OrderedCollection of: Symbol}

 	| requestName {String} requestClass {Class} |
	requestName _ self abstractTypeFor: return.
	arguments isEmpty
		ifTrue: [requestName _ requestName, 'Void']
		ifFalse:
			[arguments do: [:anArg {Symbol} |
			requestName _ requestName, (self abstractTypeFor: anArg)]].
	requestName _ requestName, 'Request'.
	requestClass _ Smalltalk at: requestName asSymbol ifAbsent: [nil].
	requestClass isNil 
		ifTrue:
			[self createRequestClass: requestName returning: return arguments: arguments.
			self compileRPCSpecialistEvaluateMsgFor: requestName returning: return arguments: arguments]
		ifFalse:
			[(requestClass inheritsFrom: Request) ifFalse: [self error: 'name collision for request subclass']]!

{void} createRequestClass: requestName {String} returning:  return {String} arguments: args {OrderedCollection of: Symbol}
	"make a Request subclass with the indicated abstract signature"
	| requestClass {Class} numArgs {IntegerVar} argList {String} requestCD {ClassDescription} |
	numArgs _ 0.
	argList _ ''.
	args do: [:anArg {Symbol} |
		numArgs _ numArgs + 1.
		argList _ argList, 'myArg', numArgs printString, ' ', (self abstractDeclarationFor: anArg)].

	requestClass _ Request subclass: requestName asSymbol
		instanceVariableNames: argList
		classVariableNames: ''
		poolDictionaries: ''
		category:  'Xanadu-Xcvr'.

	requestCD _ requestClass getCxxClassDescription.
	RPCSpecialist getCxxClassDescription fetchGroup cxxModule addClass: requestCD in: #private.

	requestCD attributes: #(CONCRETE COPY).

	requestClass compileGeneratedClassMethod:
'{BooleanVar} isGenerated
	^true'.

	self compileRequestCreateMsgIn: requestClass arguments: args.
	self compileRequestEvaluateMsgIn: requestClass returning: return arguments: args.

	requestClass compileStubbleMethods! !

!Heaper class methodsFor: 'stubble OTHERS'!

{void} compileEQ
	"Compile code to do EQ comparisions"
	self compileGeneratedMethod: 'isEqual: other ^self == other'.
	self compileGeneratedMethod: 'actualHashForEqual ^self asOop'!

{void} compileHook
	"compile changes to make this a hook class - add an instance variable and accessing methods"
	(self instVarNames includes: 'myPackagingCategory') ifFalse:
		[self addInstVarName: 'myPackagingCategory'.
		self getCxxClassDescription instVarTypeAt: 'myPackagingCategory' put: '{Category generated smalltalk}'].
	self compile: 'packagingCategory ^myPackagingCategory'
		classified: self generatedCategory notifying: nil.
	self compile: 'packagingCategory: cat myPackagingCategory := cat'
		classified: self generatedCategory notifying: nil.! !

!Heaper class methodsFor: 'stubble ON.CLIENT'!

cleanPromiseClasses
	"Heaper cleanPromiseClasses"
	ChangeLog suppressLoggingIn:
		[XuPromise allSubclassesDo: [:cl | 
			((cl class includesSelector: #makeBrokenBy:) and: [cl isNotGenerated not]) 
				ifTrue: 
					[cl selectorsDo: [:sel | cl removeSelector: sel].
					cl class selectorsDo: [:sel | cl class removeSelector: sel]]]]
					

"Remove all XuPromise subclasses named 'Pr*' from system"
	"ChangeLog suppressLoggingIn:
		[XuPromise allSubclassesDo: [:cl | 
			('Pr*' match: (cl name)  ) 
				ifTrue: 
					[cl removeFromSystem]]]"!

compileConstantPromiseMethods: promise {Class} 
	"make the methods that are the same in all promise classes.  These are:
		class methods::		make: makeBroken: makeBrokenBy:
		instance methods::	getActualFoo: getMyFoo: create: create.Problem and create.Promise"

	| myName promiseName makeMethod brokenMethod excuseMethod actualMethod createMethod createProblemMethod createPromiseMethod |
	myName _ self name.
	promiseName _ self togglePromiseName: myName. 

makeMethod _ 'make: an', myName, ' {', myName, '}
	^self create: an', myName, $..

brokenMethod _ '{', promiseName, '} makeBroken: problem {Problem star}
	| broke {', promiseName, '} |
	broke _ self create.Problem: problem.
	XuPromise savePromise: broke.
	^broke'.

excuseMethod _ '{', promiseName, '} makeBrokenBy: broken {XuPromise}
	^self create.Promise: broken.'.

actualMethod _ '{', myName, '} getActual', myName, '
	self isBroken
		ifFalse: [^super actualThing cast: ', myName, '.]
		ifTrue: [Heaper BLAST: #BrokenPromise].'.

createMethod _ 'create: an', myName, ' {', myName, '}
	super create: an', myName, '.'.

createProblemMethod _ 'create.Problem: problem {Problem}
	super create.Problem: problem.'.

createPromiseMethod _ 'create.Promise: promise {XuPromise}
	super create.Promise: promise.'.

	promise class compile: makeMethod
		classified: 'pseudo-constructors'
		notifying: nil.
	promise class compile: brokenMethod
		classified: 'pseudo-constructors'
		notifying: nil.
	promise class compile: excuseMethod
		classified: 'pseudo-constructors'
		notifying: nil.
	promise compile: actualMethod
		classified: 'promise access'
		notifying: nil.
	promise compile: createMethod
		classified: #creation
		notifying: nil.
	promise compile: createProblemMethod
		classified: #creation
		notifying: nil.
	promise compile: createPromiseMethod
		classified: #creation
		notifying: nil.!

compilePromise
	"make a promise class and generate all its methods."
	
	self == Heaper ifTrue: [^VOID].  "hack so we can have a Heaper ON.CLIENT declaration."
	self cachePromiseNameTableIn: 
	[| promise {Class} |
	promise _ self compilePromiseClass.
	self compileConstantPromiseMethods: promise.
	self compilePromiseMethods: promise.
	self compilePromiseFluidDeclarations: promise]!

{Class} compilePromiseClass
	"make a promise subclass with its general messages and return it"

	| promise {Class} pname {Symbol}  |
	pname _ self promiseName. 
	promise _ self superclass promiseClass subclass: pname
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self category
		attributes: #(CONCRETE)
		comment: self comment.


	 (self category = #'Xanadu-calc') ifTrue: [
		(CxxSystemOrganization getOrMakeFileNamed: 'prcalc')  addClass: promise getOrMakeCxxClassDescription in: #public.]
	ifFalse: [
		(CxxSystemOrganization getOrMakeFileNamed: 'promise')  addClass: promise getOrMakeCxxClassDescription in: #public.].

	"file _ CxxSystemOrganization fetchTreeLocForFile: self getCxxClassDescription fetchGroup cxxModule moduleName.
	 Nothing more expected ->[file value == #dir or: [file value isNil]] whileFalse:
		[file _ file parent]."

	 ^promise!

compilePromiseDefaultMethods: promise {Class} with: selector {Symbol} with: count {Integer}
	"compile a default for the indicated promise method stripping off the last count arguments"

	|  pattern returnType params method source {String} keywords |
	source _ self sourceCodeStringAt: selector.
	pattern _ Parser new parsePattern: source.
	returnType _ self togglePromiseOfParse: (pattern at: 3).
	params _ pattern at: 2.
	keywords _ selector keywords.
	method _ String streamContents: 
		[:oo |
		oo << ${ << returnType << '} '.
		params size = count ifTrue:
			[ | sel |
			sel _ keywords at: 1.
			oo << (sel copyFrom: 1 to: sel size - 1)].
		1 to: params size - count do:
			[:index | | keyword param |
			keyword _ keywords at: index.
			param _ params at: index.
			oo << keyword << ' ' << param name.
			oo << ' {' <<  (self togglePromiseOfParse: param type) << '} '].
		oo << '

	^self '.
		1 to: params size - count do:
			[:index | | keyword param |
			keyword _ keywords at: index.
			param _ params at: index.
			oo << ' ' << keyword << ' ' << param name].
		params size - count + 1 to: params size do:
			[:index | | keyword param   |
			keyword _ keywords at: index.
			param _ params at: index.
			oo << ' ' << keyword << ' '.
			oo << (Heaper promiseDefaultValue: (TypeDescription fromParse: param type) default with: selector)]].
	promise compile: method
		classified: 'smalltalk: defaults'
		notifying: nil.
	(params size > count and:
			[(TypeDescription fromParse: (params at: params size - count) type) default notNil]) ifTrue:
		[self compilePromiseDefaultMethods: promise with: selector with: count + 1]!

compilePromiseFluidDeclarations: promise {Class}
	"if there's a fluid declaration in the staticTimeNonInherited, then declare the client counterpart" 

	| pNode method | 
	(self class includesSelector: #staticTimeNonInherited) ifFalse: [^nil].
	pNode _ (Parser new
		parse: (self class sourceCodeStringAt: #staticTimeNonInherited)
		class: self class
		notifying: nil).
	method _ String streamContents: 
		[:oo | 
		oo << 'staticTimeNonInherited
'.
		pNode block body statements do: [:stmt | 
			stmt selector == #defineClientFluid:with:with: ifTrue:
				[| fluidName |
				fluidName _ (stmt arguments at: 1) value.
				oo << '
	'.
				oo << (self togglePromiseName: stmt receiver name asSymbol).
				oo << ' definePromiseFluid: #Xu' << fluidName.
				oo << ' with: XuPromise emulsion with: [NULL] with: #' << fluidName.
				oo << ' with: ' << stmt receiver name << $.]]].


	method size > 23 ifTrue:
		[promise class compile: method
			classified: 'smalltalk: initialization'
			notifying: nil]!

compilePromiseMethods: promise {Class}
	"update return types, argument types, mangling and unmangling information"

	| protocol pattern returnType params method actualReturnType | 
	self selectors do: 
		[ :selector |
		(self selector: selector hasAttribute: #CLIENT) ifTrue:
			[| source {String} comments {OrderedCollection of: String} comment {String} |
			source := self sourceCodeStringAt: selector.
			protocol _ (self organization categoryOfElement: selector) asSymbol.
			comments _ Parser new parseMethodComment: source setPattern: [:pat | pattern _ pat].
			comments isEmpty
				ifTrue: [comment _ 'This method has no comment.']
				ifFalse: [comment _ comments at: 1].
			returnType _ self togglePromiseOfParse: (actualReturnType _ pattern at: 3).
			actualReturnType _ TypeDescription fromParse: actualReturnType.
			actualReturnType _ actualReturnType baseType ~~ nil ifTrue: [actualReturnType baseType asSymbol] ifFalse: ['Nil' asSymbol].
			params _ pattern at: 2.
			method _ String streamContents: 
				[:oo |				"generate a method in the promise class"
				oo << ${ << returnType << '} '.

				params isEmpty 		"generate the method signature"
					ifTrue: [oo << selector]
					ifFalse:
						[selector keywords with: params do: 
							[:keyword :param |
							oo << keyword << ' ' << param name.
							oo << ' {' <<  (self togglePromiseOfParse: param type) << '} ']].
				oo << '

	"' << comment << '"
	
	| retVal {' << returnType << '} |
	(self isBroken) ifTrue:
		[^' << returnType << ' makeBrokenBy: self].'.

				params isEmpty ifFalse:	"generate the isBroken test for each argument"
					[selector keywords with: params do: 
						[:keyword :param |
						| type |
						type _ TypeDescription fromParse: param type.
						((self isRawType: type) or: [type isPromise]) ifFalse:
							[oo << '
	(' << param name << ' isBroken) ifTrue:
		[^' << returnType << ' makeBrokenBy: ' << param name << '].']]].
				oo << '

	Heaper problems.AllBlasts 
		handle: [:ex | retVal _ ' << returnType << ' makeBroken: ex asProblem. ex return]
		do: 	[retVal _ ' << returnType. 
		(returnType = #XuVoid)
			ifTrue: [oo << ' make.']
			ifFalse: [
				actualReturnType = #BooleanVar
					ifTrue: [oo << ' booleanVar:']
					ifFalse: [(self isIntType: actualReturnType)
								ifTrue: [oo << ' integerVar:']
								ifFalse: [oo << ' make:']]].
		oo << ' (self getActual' << self name.

				params isEmpty 			"generate the argument list in the call on the actuals"
					ifTrue: [oo << ' ' << selector]
					ifFalse:
						[selector keywords with: params do: 
							[:keyword :param | | type |
							oo << ' ' << keyword.
							type _ TypeDescription fromParse: param type.
							type isPromise
								ifTrue: [oo << ' (' << type baseType << ' make: ' << param name << $)]
								ifFalse: 
									[(self isRawType: type) 
										ifTrue: [oo << ' ' << param name]
									ifFalse: [type isPointer not
										ifTrue: [oo <<param name << ' getActual' << type baseType ]
									ifFalse:
										[oo << ' ((XuPromise getActual: ' << param name << ') cast: '.
										oo << type baseType << ')']]]]].

				oo << ')].
	XuPromise inDelayBlock ifFalse:
		[XuPromise force.
		retVal isBroken ifTrue: [retVal explode]].
	^retVal'].

			promise compile: method
				classified: protocol
				notifying: nil.
			(params isEmpty not and: [(TypeDescription fromParse: params last type) default notNil]) ifTrue:
				[self compilePromiseDefaultMethods: promise with: selector with: 1]]]!

isIntType: type {Symbol}
	^ #(IntegerVar Int32 UInt32 Int8 UInt8) includes: type!

{BooleanVar} isRawType: type {TypeDescription}

	^type refType = #star and: [type baseType = 'char' or: [type baseType = 'void']]!

{String} promiseDefaultValue: default with: selector
	| defaultValue {String} |
	defaultValue _ nil.
	default isVarNode
		ifFalse: [default value = false
					ifTrue: [defaultValue _ 'DefaultFalse']
					ifFalse: [default value = true
								ifTrue: [defaultValue _ 'DefaultTrue']
								ifFalse: [default value = -1 ifTrue: [defaultValue _ 'DefaultMinusOne']]]]
		ifTrue:
			[default name = 'Int32Zero' ifTrue: [defaultValue _ 'DefaultZero'].
			default name = 'NULL' ifTrue: [defaultValue _ 'DefaultNull']].
	defaultValue isNil
		ifTrue: [self BLAST: 'Unrecognized default in ', self name, '>>', selector]
		ifFalse: [^ defaultValue].! !

!Heaper class methodsFor: 'promise name table'!

{IdentityDictionary} cachePromiseNameTable
	"build a pair of tables from promise names to backend names and back."
	"Heaper cachePromiseNameTable"

	PromiseNameTable _ IdentityDictionary new: 512.
	Heaper generatePromiseNames: nil.

	PromiseNameTable at: #XuPromise put: #Heaper.
	PromiseNameTable at: #Heaper put: #XuPromise.

	PromiseNameTable at: #XuVoid put: #void.
	PromiseNameTable at: #void put: #XuVoid.
	PromiseNameTable at: #NOWAIT put: #XuVoid.
	PromiseNameTable at: #NOACK put: #XuVoid.

	PromiseNameTable at: #BooleanVar put: #XuIntValue.
	PromiseNameTable at: #XuBooleanValue put: #BooleanVar.
 
	PromiseNameTable at: #XuValue put: #PrimValue.
	PromiseNameTable at: #PrimValue put: #XuValue.
	
	PromiseNameTable at: #XuIntValue put: #PrimIntValue.
	PromiseNameTable at: #PrimIntValue put: #XuIntValue.

	PromiseNameTable at: #XuFloatValue put: #PrimFloatValue.
	PromiseNameTable at: #PrimFloatValue put: #XuFloatValue.
	
	PromiseNameTable at: #XuArray put: #PrimArray.
	PromiseNameTable at: #PrimArray put: #XuArray.

	PromiseNameTable at: #IntegerVar put: #XuIntValue.
	PromiseNameTable at: #Int32 put: #XuIntValue.
	PromiseNameTable at: #Int8 put: #XuIntValue.
	PromiseNameTable at: #UInt8 put: #XuIntValue.
	PromiseNameTable at: #UInt32 put: #XuIntValue.
	
	PromiseNameTable at: #IEEE128 put: #XuFloatValue.
	PromiseNameTable at: #IEEE64 put: #XuFloatValue.
	PromiseNameTable at: #IEEE32 put: #XuFloatValue.
	PromiseNameTable at: #IEEE8 put: #XuFloatValue.!

cachePromiseNameTableIn: aBlock
	"evaluate a block with the cache enabled"
	PromiseNameTable notNil
		ifTrue: [aBlock value]
		ifFalse:
			[self cachePromiseNameTable.
			aBlock valueNowOrOnUnwindDo:
				[self flushPromiseNameTable]]!

{String} exportName
	"Return the completely stripped rootName for this class.  Get rid of prefixes. 
	 Some classes will implement this directly in order to provide another name."

	(self == Heaper or: [self hasAttribute: #ON.CLIENT]) ifTrue: [^self rootName].
	^self superclass exportName!

{IdentityDictionary} flushPromiseNameTable
	"flush the promise name table."
	"Heaper flushPromiseNameTable"

	PromiseNameTable _ nil!

{void} generatePromiseNames: promise {Symbol | nil}
	"Walk down the inheritance tree recording what promise name goes with which class.  The incoming promise name is used by any classes which are not ON.CLIENT because instances of them will be known to the Clients as that type."
	"PromiseNameTable _ IdentityDictionary new: 512.
	 Heaper generatePromiseNames: nil."
	
	| pname |
	pname _ promise.
	(self hasAttribute: #ON.CLIENT)
			ifTrue: 
				[pname _ self promiseName.
				PromiseNameTable at: pname put: self name.
				PromiseNameTable at: self name put: pname.
				"PromiseNameTable at: self rootName asSymbol put: self name"]
			ifFalse:
				[(pname ~~ nil and: [(self hasAttribute: #NOT.A.TYPE) not]) 
					ifTrue: [PromiseNameTable at: self name put: pname]].
	self subclassesDo: [:cl | cl generatePromiseNames: pname]!

{Symbol} parseExportName: pNode {ProgramNode}  
	| type {TypeDescription} sym {Symbol} |
	type _ TypeDescription fromParse: pNode.
	sym _ type baseType asSymbol.
	(sym = #char and: [type refType = #star]) ifTrue: [^#'char star']
	ifFalse: [(sym = #void and: [type refType = #star]) ifTrue: [^#'void star']
	ifFalse: [sym = #void ifTrue: [^'Void']
	ifFalse: [sym = #NOACK ifTrue: [^'NOACK']
	ifFalse: [^(Smalltalk at: sym ifAbsent: [self warn: 'Symbol not client or promise: ', sym, ' in: ' , self name. Heaper]) exportName]]]]!

{Class} promiseClass
	"starting at self and moving up the hierarchy, find the first ON.CLIENT class, and return its promise counterpart.  
	BLAST if it should exist and doesn't.  "
	"?? returns nil if it should but there's something else in the global dictionary by that name.???  "

	|  promiseTable |
	promiseTable _ self promiseNameTable.
	^Smalltalk at: (promiseTable at: self name ifAbsent: [#XuPromise])!

{Symbol} promiseName
	"Return the name of the promise class associated generated for 'name'."

	^('Xu' , self exportName) asSymbol!

{IdentityDictionary} promiseNameTable
	Heaper cachePromiseNameTableIn: [^PromiseNameTable]!

{String} rootName
	"Return the completely stripped rootName for this class.  Get rid of prefixes. 
	 Some classes will implement this directly in order to provide another name."

	name size < 3 ifTrue: [^name asString].
	"(name testPrefix: 'Prim') 
		ifTrue: [^self name copyFrom: 5]."
	self == Heaper ifTrue: [^'Promise'].
	(name testPrefix: 'Prim') 
		ifTrue: [^self name copyFrom: 5].
	(name at: 3) isUppercase ifFalse: [^name asString].
	((name testPrefix: 'Fe') 
		or: [(name testPrefix: 'Xu')
		or: [(name testPrefix: 'Xn')
		or: [name testPrefix: 'Pr']]])
		ifTrue: [^self name copyFrom: 3].
	^name asString!

{String} serverNameFor: root {String}
	"Return the name of the server side class that implements 'root'"

	root = #NOACK ifTrue: [^'NOACK'].
	(root testSuffix: 'OrNull')
		ifTrue: [^self promiseNameTable at: ('Xu' , (root copyFrom: 1 to: (root size - 6))) asSymbol]
		ifFalse: [^self promiseNameTable at: ('Xu' , root) asSymbol]!

{Symbol} togglePromiseName: sym {Symbol}  
	Heaper cachePromiseNameTableIn:
		[^PromiseNameTable at: sym ifAbsent: [self warn: 'Symbol not client or promise: ', sym, ' in: ' , self name.  #Heaper]]!

{Symbol} togglePromiseOfParse: pNode {ProgramNode}  
	| type {TypeDescription} sym {Symbol} default  |
	type _ TypeDescription fromParse: pNode.
	sym _ type baseType ~~ nil ifTrue: [type baseType asSymbol] ifFalse: ['Nil' asSymbol].
	default _ type default.
	default ~~ nil
		ifTrue: [default _ self promiseDefaultValue: default with: #default]
		ifFalse: [default _ nil].
	
	(sym = #char and: [type refType = #star])
		ifTrue: [^#'char star']
		ifFalse: [(sym = #void and: [type refType = #star])
			ifTrue: [^#'void star']
			ifFalse: 
				[Heaper cachePromiseNameTableIn:
					[|prThing|
					prThing _ PromiseNameTable at: sym
						ifAbsent: [self warn: 'Symbol not client or promise: ', sym, ' in: ' , self name].
					default ~~ nil
						ifTrue: 
							[prThing isBehavior ifTrue: [ prThing _ prThing name].
							^ ((prThing asString), ' default: ', default) asSymbol ]
						ifFalse: [^ prThing ]]]]! !

!Heaper class methodsFor: 'client generation'!

clientProtocol: ii do: block
	"| file |
	file _ (Filename named: 'client.prc') readStream.
	[Heaper clientProtocol: file do: 
		[:number :class :parent :implicit :meth |
		cerr << number << ' ' << class.
		meth value: 
			[:mnum :return :sel :args :static | sel = #import ifTrue: [self halt].
			cerr cr << mnum << ' ' << return << ' ' << sel << $( << args << $)].
		cerr cr]] valueNowOrOnUnwindDo: [file close]"
	
	[ii skipSeparators atEnd] whileFalse:
		[| tok |
		tok _ ii nextToken.
		tok = #class ifTrue: 
			[| array implicit |
			ii skipThrough: Character space.
			"| number name parent implicit |"
			array _ Array new: 5.
			array at: 1 put: (Integer readFrom: ii).
			array at: 2 put: ii nextToken. 
			array at: 3 put: ii nextToken.
			ii skipSeparators.
			(ii peekFor: ${) 
				ifTrue: [implicit _ false]
				ifFalse: [(implicit _ (tok _ ii nextToken) sameAs: #implicit) ifFalse: [self halt: 'Unrecognized flag: ' , tok]].
			array at: 4 put: implicit.
			ii skipThrough: Character cr.
			array at: 5 put: 
				[:mblock |
				[ii peekFor: $}] whileFalse:
					[| static tmp |
					tok _ ii nextToken.
					((static _ tok = #function) or: [tok = #message]) ifTrue: 
						[| args marray |
						ii skipSeparators.
						marray _ Array new: 5.
						marray at: 1 put: (Integer readFrom: ii).
						marray at: 2 put: ii nextToken.
						marray at: 3 put: ii nextToken.
						(tmp _ ii next) == $( ifFalse: [self halt: 'Confused.'].
						args _ OrderedCollection new.
						[ii peekFor: $)] whileFalse:
							[ii peekFor: $,.
							args add: ii nextToken.
							ii skipSeparators.
							(ii peek == $)) ifFalse: [ii nextToken]].
						marray at: 4 put: args.
						marray at: 5 put: static.
						mblock valueWithArguments: marray]
					ifFalse: 
						[(tok = #constant or: [tok = #event or: [tok = #response]]) ifFalse: [self halt: 'missing request.']].
					ii skipThrough: Character cr]].
			block valueWithArguments: array]
		ifFalse: 
			[ii skipThrough: Character cr]]!

compilePromiseHandlers
	ChangeLog suppressLoggingIn:
		[| mod {CxxModule | nil} pg {CxxClassGroup | nil} sigs |
		PromiseManager class removeCategory: #'translate: generated'.
		mod _ CxxSystemOrganization fetchModuleNamed: 'handlrs'.
		mod ~~ nil ifTrue: [pg _ mod fetchPublicGroup].
		pg ~~ nil ifTrue: [pg hxxHeader: ''].
		
		sigs _ self handlerSignaturesFrom: 'client.prc'.
		sigs asSet do: 
			[:sig |
			self normalRequestHandler: (sig at: 1) with: (sig at: 2)]]!

handlerSignaturesFrom: filename
	| file  abstracts |
	abstracts _ Bag new.
	file _ (Filename named: filename) readStream.
	[Heaper clientProtocol: file do: 
		[:number :class :parent :implicit :meth |
		meth value: 
			[:num :return :sel :args :static | 
			args size > 6 ifTrue: 
				[cerr << class << '::' << sel << '('.
				args do: [:arg | cerr << arg << ', '].
				cerr << ')'.
				Transcript cr; endEntry].
			(implicit or: [static]) ifFalse: [args addFirst: class].
			abstracts add: (Array 
					with: (self promiseToAbstract: return) 
					with: (args asArray collect: [:arg | self promiseToAbstract: arg]) 
					"with: ((implicit or: [static]))")].
		]] valueNowOrOnUnwindDo: [file close].
	^abstracts!

makeClassTable: filename
	| file   source |
	file _ (Filename named: filename) readStream.
[	self cachePromiseNameTableIn:
[	source _ String streamContents: [:oo |
		oo << '{void} fillClassTable: table {PtrArray}
	'.
		Heaper clientProtocol: file do: 
			[:number :class :parent :implicit :meth |
			class ~= 'Void' ifTrue:
				[oo crtab << 'table at: ' << number << ' storeValue: ' 
					<< (self serverNameFor: class) << $.]]].
	ChangeLog suppressLoggingIn:
		[PromiseManager class compile: source classified: #'translate: generated' notifying: nil].
]]	 valueNowOrOnUnwindDo: [file close].!

makeRequestTable: filename
	| file  i oo |
	file _ (Filename named: filename) readStream.
[	self cachePromiseNameTableIn:
[	i _ 1.
	oo _ (String new: 1000) writeStream.
	oo << '{void} fillRequestTable: table {PtrArray}
	'.
	Heaper clientProtocol: file do: 
		[:number :class :parent :implicit :meth |
		oo cr cr << '"Requests for class ' << class << '"'.  oo cr.
		meth value: 
			[:num :return :sel :args :static |
			| types override  |
			i \\ 80 == 0 ifTrue: "100 was too big.  -- cth"
				["Compile the current method and start another." 
				oo crtab << 'self fillRequestTable' << (i // 80) << ': table.'.
				ChangeLog suppressLoggingIn:
					[PromiseManager class compile: oo contents classified: #'translate: generated' notifying: nil].
				oo reset.
				oo << '{void} fillRequestTable' << (i // 80) << ': table {PtrArray}'.
				oo crtab].
			i _ i + 1.
			oo crtab << 'table at: ' << num << ' storeValue: 
		('. "self halt."
			(override _ PromiseManager mapOverride: sel with: class with: args count) ~~ nil ifTrue:
				[oo << 'SpecialHandler make: (PromiseManager pointerToStaticMember: #'.
				oo << override << ' with: ''VHFn'')']
		ifFalse:
			[| sig {String} |
			types _ args copy.
			implicit | static ifFalse: [types addFirst: class].
			sig _ String streamContents: [:o2 |
				o2 << ((self promiseToAbstract: return) at: 1).
				types do: [:type | | abs |
					abs _ self promiseToAbstract: type.
					(abs testSuffix: 'OrNull') 
						ifTrue: [o2 << 'N']
						ifFalse: [o2 << ((self promiseToAbstract: type) at: 1)]]].
			oo << sig << 'Handler make: (RequestHandler pointerToStaticMember: #'.
			oo << class << '.U.' << sel << '.U.N' << types size.
			types isEmpty ifFalse: 
				[oo << $:.
				types size -1 timesRepeat: [oo << 'with:']].
			oo << ' with: ''' << sig << 'Fn'')'.
			types do: [:type |
				((self promiseToAbstract: type) testPrefix: 'Heaper')
					ifTrue: [(oo crtab: 3) << 'with: ' << (self serverNameFor: type)]]].
			oo << ').']].
	ChangeLog suppressLoggingIn:
		[PromiseManager class compile: oo contents classified: #'translate: generated' notifying: nil].
]]	 valueNowOrOnUnwindDo: [file close].
	 
"fillTableN: table
	table at: 1 store:
		[:pm {PromiseManager} |
		pm requestHHI: 
				[:arg1 {Heaper} :arg2 {IntegerVar} |
				arg1 get: arg2]
			with: PrimArray]."!

normalRequestHandler: return with: args

	| interval  sig rclass source mod {CxxModule} pg |
	interval _ 1 to: args size.
	sig _ String streamContents: [:oo | oo << (return at: 1).
			args do: [:arg |
					(arg testSuffix: 'OrNull') 
						ifTrue: [oo << 'N']
						ifFalse: [oo << (arg at: 1)]]].
	rclass _ RequestHandler subclass: (sig, 'Handler') asSymbol
		instanceVariableNames: 
			(String streamContents: [:oo |
				oo << 'myFn {' << sig << 'Fn var}'.
				args with: interval do: 
					[:arg :i | (arg testPrefix: 'Heaper') ifTrue: [oo << ' myType' << i << ' {Category}']]])
		classVariableNames: '' poolDictionaries: '' category: 'Xanadu-proman' 
		attributes: #(CONCRETE NOT.A.TYPE) comment: ''.
	mod _ CxxSystemOrganization fetchModuleNamed: 'handlrs'.
	mod ~~ nil assert: 'Must have a module named handlrs'.
	mod addClass: rclass getCxxClassDescription in: #public.
	pg _ mod fetchPublicGroup.
	pg hxxHeader: pg hxxHeader, (String streamContents: [:oo |
		oo << 'typedef '.
		(return testPrefix: #Heaper) ifTrue: [oo << 'SPTR(Heaper)'].
		return = #Void ifTrue: [oo << 'void'].
		return = #IntegerVar ifTrue: [oo << 'IntegerVar'].
		return = #BooleanVar ifTrue: [oo << 'BooleanVar'].
		oo << ' (*' << sig << 'Fn' << ') ('.
		args 
			do: [:arg |
				(arg testPrefix: #Heaper) ifTrue: [oo << 'APTR(Heaper)'].
				arg = #IntegerVar ifTrue: [oo << 'IntegerVar'].
				arg = #BooleanVar ifTrue: [oo << 'BooleanVar']]
			andBetweenDo: [oo << ', '].
		oo << ');'; cr]).
	
	source _ String streamContents: [:oo |
		oo << '{void} handleRequest: pm {PromiseManager}'.
		oo crtab.
		args isEmpty ifFalse:
			[oo crtab << $|.
			args with: interval do: 
				[:arg :i |
				oo << ' arg' << i << ' {'.
				(arg testSuffix: 'OrNull')
					ifTrue: [oo <<  'Heaper | NULL']
					ifFalse: [oo << arg].
				oo << $}].
			oo << ' |'].
		oo cr.
		args with: interval do: 
			[:arg :i |
			oo tab << 'arg' << i << ' _ pm '.
			arg = #HeaperOrNull ifTrue: [oo << 'fetchHeaper: myType' << i].
			arg = #Heaper ifTrue: [oo << 'fetchNonNullHeaper: myType' << i].
			arg = #IntegerVar ifTrue: [oo << 'fetchIntegerVar'].
			arg = #BooleanVar ifTrue: [oo << 'fetchBooleanVar'].
			oo << $.; cr].
		oo << 
'	pm noErrors ifTrue:
		['.
		return = #Heaper ifTrue: [oo << 'pm respondHeaper: '].
		return = #IntegerVar ifTrue: [oo << 'pm respondIntegerVar: '].
		return = #BooleanVar ifTrue: [oo << 'pm respondBooleanVar: '].
		oo << $( << 'myFn invokeFunction'.
		args with: interval do: 
			[:arg :i | i = 1 ifTrue: [oo << ': arg1'] ifFalse: [oo << ' with: arg' << i]].
		oo << $).
		return = #Void ifTrue: [oo << '.
		pm respondVoid'].
		oo << $]].
	rclass compile: source classified: #'request handling' notifying: nil.

	source _ String streamContents: [:oo |
		oo << 'create: fn {' << sig << 'Fn var}'.
		args isEmpty ifFalse:
			[args with: interval do: 
				[:arg :i | (arg testPrefix: 'Heaper') ifTrue: [oo << ' with: type' << i << ' {Category}']]].
		oo crtab << 'super create.'.
		oo crtab << 'myFn _ fn.'.
		args isEmpty ifFalse:
			[args with: interval do: 
				[:arg :i | (arg testPrefix: 'Heaper') ifTrue: [oo crtab << 'myType' << i << ' _ type' << i << $.]]]].
	rclass compile: source classified: #creation notifying: nil.

	source _ String streamContents: [:oo |
		oo << '{RequestHandler} make: fn {' << sig << 'Fn var}'.
		args isEmpty ifFalse:
			[args with: interval do: 
				[:arg :i | (arg testPrefix: 'Heaper') ifTrue: [oo << ' with: type' << i << ' {Category}']]].
		oo crtab << '^self create: fn'.
		args isEmpty ifFalse:
			[args with: interval do: 
				[:arg :i | (arg testPrefix: 'Heaper') ifTrue: [oo << ' with: type' << i]]]].
	rclass class compile: source classified: #creation notifying: nil.
	
	rclass compileGeneratedClassMethod: 'isGenerated ^true'!

promiseToAbstract: token
	"token = #IntValue 		ifTrue: [^#IntegerVar]."
	token = #BooleanValue	ifTrue: [^#BooleanVar].
	token = #Void 			ifTrue: [^#Void].
	(token testSuffix: 'OrNull') ifTrue: [^#HeaperOrNull].
	^#Heaper!

requestProcedure: sel with: class with: return with: args with: implicit with: static
	"Heaper requestProcedure: 'position' with: 'IntegerSpace' with: 'Integer' with: #('IntValue') with: true with: true."
	"Heaper requestProcedure: 'count' with: 'Edition' with: 'IntValue' with: #() with: false with: false."

	|  selector source checkTypes conversions returnConversion |
	(PromiseManager mapOverride: sel with: class with: args count) ~~ nil ifTrue: [^VOID].
	source _ String streamContents: [:oo |
		selector _ self smalltalkSelector: sel with: class with: args count.
		oo << ${ << (self serverNameFor: return) << '} ' << class << '.U.' << sel << '.U.N'.
		checkTypes _ return = 'IntValue' | (return = 'FloatValue').
		implicit | static ifTrue:
			[oo << args size]
		ifFalse: 
			[oo << (args size + 1) << ': receiver {' << (self serverNameFor: class) << '}'.
			args isEmpty ifFalse: [oo << ' with']].
		args with: (1 to: args size) do: [:type :i | 
			i = 1 ifTrue: [oo << ': '] ifFalse: [oo << ' with: '].
			oo << 'arg' << i << ' {' << (self serverNameFor: type) << '}'.
			type = 'IntValue' | (type = 'FloatValue') ifTrue: [checkTypes _ true]].
		conversions _ Array new: args count.
		returnConversion _ nil.
		checkTypes ifTrue:
			[| realClass pattern params |
			realClass _ Smalltalk at: (self serverNameFor: class).
			static ifTrue: [realClass _ realClass class].
			pattern _ Parser new parsePattern: (realClass sourceCodeStringAt: selector).
			params _ pattern at: 2.
			1 to: args count do: [:i |
				(args at: i) = 'IntValue' | ((args at: i) = 'FloatValue') ifTrue: 
					[| type |
					type _ (TypeDescription fromParse: (params at: i) type) baseType.
					(type ~= 'PrimIntValue' and: [type ~= 'PrimFloatValue']) ifTrue: [conversions at: i put: type]]].
			return = 'IntValue' | (return = 'FloatValue') ifTrue:
				[| type |
				type _ (TypeDescription fromParse: (pattern at: 3)) baseType.
				(type ~= 'PrimIntValue' and: [type ~= 'PrimFloatValue']) ifTrue: [returnConversion _ type]]].
		oo crtab.
		return = #Void ifFalse: [oo << $^].
		returnConversion ~~ nil 
			ifTrue: [oo << (self serverNameFor: return) << ' make: ('].
		implicit | static
			ifTrue: 
				[static 
					ifTrue: [oo << (self serverNameFor: class)]
					ifFalse: [oo << (self serverNameFor: class) << ' implicitReceiver']]
			ifFalse: [oo << 'receiver'].
		args isEmpty 
			ifTrue: [oo space << selector]
			ifFalse: 
				[selector keywords
					with: (1 to: args size) 
					do: [:key :i | oo space << key << ' arg' << i.
						(conversions at: i) notNil: [:con | oo << ' as' << con]]].
		returnConversion ~~ nil ifTrue: [oo << ')']].
	ChangeLog suppressLoggingIn:
		[RequestHandler class compile: source classified: 'translate: generated' notifying: nil]
	
"{FeEdition} RequestHandler::Edition.U.combine: arg1 {FeEdition} with: arg2 {FeEdition}
	^arg1 combine: arg2"!

requestProceduresFrom: filename
	| file  | 
	file _ (Filename named: filename) readStream.
[	self cachePromiseNameTableIn:
[	Heaper clientProtocol: file do: [:number :class :parent :implicit :meth |
		meth value: [:num :return :sel :args :static |
			self requestProcedure: sel 
				with: class 
				with: return 
				with: args 
				with: implicit 
				with: static
 ]]]]	 valueNowOrOnUnwindDo: [file close].


"{FeEdition} RequestHandler::Edition.U.combine: arg1 {FeEdition} with: arg2 {FeEdition}
	^arg1 combine: arg2"!

smalltalkSelector: stripped with: class with: argCount
	| selector | 
	selector _ stripped.
	class = 'Array' ifTrue:
		['store' = stripped ifTrue: [selector _ 'at:storeValue']
		ifFalse: ['storeMany' = stripped ifTrue: [selector _ 'at:storeMany']
		ifFalse: ['get' = stripped ifTrue: [selector _ 'getValue']]]].
	^(String streamContents: [:oo | oo << selector.
		argCount > 0 ifTrue: [oo << $:].
		stripped == selector
			ifTrue: [argCount-1 timesRepeat: [oo << 'with:']]
			ifFalse: [argCount-2 timesRepeat: [oo << 'with:']]]) asSymbol! !

!Heaper class methodsFor: 'client freeze'!

allClientProtocolOn: oo
	"| file |
	file _ (Filename named: 'proto.spc') writeStream.
	[Heaper allClientProtocolOn: file]
		valueNowOrOnUnwindDo: [file close]"
	"Write out all the client protocol."
	
	NextClientRequestNumber _ 1.
	Heaper cachePromiseNameTableIn: 
		[| i |
		i _ 1.
		self frozenClasses do: 
			[:pr | 
			(Smalltalk at: (self serverNameFor: pr asSymbol)) clientProtocolOn: oo with: i.
			i _ i + 1].
		]!

clientClassesDo: block
	"Heaper clientClassesDo: [:assoc | Transcript << assoc key << ' ']. Transcript endEntry"
	
	| sorted recur |
	block value: self exportName -> self.
	sorted _ SortedCollection new.
	recur _ [:cl | (cl hasAttribute: #ON.CLIENT) 
					ifTrue: [sorted add: cl exportName -> cl]
					ifFalse: [cl subclassesDo: recur]].
	self subclassesDo: recur.
	sorted do: [:assoc | assoc value clientClassesDo: block]!

clientProtocol
	"IntegerSpace clientProtocol"
	"Compute the client protocol."
	
	^String streamContents: [:oo | self clientProtocolOn: oo]!

fileOutClientProtocol: filename
	| file |
	PromiseNameTable _ nil.
	file _ (Filename named: filename) writeStream.
	[Heaper allClientProtocolOn: file]
		valueNowOrOnUnwindDo: [file close]!

freezeClientClasses
	"Heaper freezeClientClasses."
	
	(self confirm: 'Do you really want to do this?') ifFalse: [^VOID].
	(self confirm: 'Are you sure?') ifFalse: [^VOID].
	Heaper cachePromiseNameTableIn: 
	[Heaper at: #clientClasses storeInfo: 
		(String streamContents: [:oo |
			self clientClassesDo: [:assoc | oo << assoc key << ' ']])]!

freezeClientProtocol
	"Heaper freezeClientProtocol."
	
	(self confirm: 'Do you really want to do this?') ifFalse: [^VOID].
	(self confirm: 'Are you sure?') ifFalse: [^VOID].
	self withAllSubclasses do: [:class | 
		(class hasAttribute: #ON.CLIENT) ifTrue:
			[class at: #frozenProtocol storeInfo: class clientProtocol]]!

freezeStProtocol
	"Heaper freezeStProtocol."
	
	(self confirm: 'Do you really want to do this?') ifFalse: [^VOID].
	(self confirm: 'Are you sure?') ifFalse: [^VOID].
	self withAllSubclasses do: [:cl | 
		(cl hasAttribute: #ON.CLIENT) ifTrue:
			[cl at: #stProtocol storeInfo: cl stClientProtocol]]!

stClientProtocol
	"IntegerSpace stClientProtocol"
	"Compute the client protocol."
	
	^String streamContents: 
		[:oo | 
		self class selectors asSortedCollection do: 
			[:sel | 
			(self class selector: sel hasAttribute: 'CLIENT') ifTrue:
				[| pat type args |
				pat _ Parser new parsePattern: (self class sourceCodeStringAt: sel).
				type _ pat at: 3.
				args _ pat at: 2.
				oo << type.
				args isEmpty 
					ifTrue: [oo << ' ' << (pat at: 1)]
					ifFalse: [(pat at: 1) keywords with: args do: 
							[:key :arg | oo << ' ' << key << ' '.  arg printOn: oo indent: 0]].
				oo cr]].
		self selectors asSortedCollection do: 
			[:sel | 
			(self selector: sel hasAttribute: 'CLIENT') ifTrue:
				[| pat type args |
				pat _ Parser new parsePattern: (self sourceCodeStringAt: sel).
				type _ pat at: 3.
				args _ pat at: 2.
				oo << type.
				args isEmpty 
					ifTrue: [oo << ' ' << (pat at: 1)]
					ifFalse: [(pat at: 1) keywords with: args do: 
							[:key :arg | oo << ' ' << key << ' '.  arg printOn: oo indent: 0]].
				oo cr]]]!

verifyFreeze
	"Heaper verifyFreeze."
	
	| clients passed |
	clients _ self frozenClasses asSet.
	passed _ true.
	self withAllSubclasses do: [:class |
		(class hasAttribute: #ON.CLIENT) ifTrue:
			[clients remove: class name asString 
				ifAbsent: [passed _ false. self warn: 'Added client class: ', class name].
			(class getInfo: #frozenProtocol) = class clientProtocol 
				ifFalse: [passed _ false. self warn: 'Different client methods: ', class name]]].
	clients isEmpty ifFalse: [passed _ false. self warn: 'Removed client classes: ', clients].
	passed ifTrue: [Transcript cr; show: 'Protocol verification passed.']! !

!Heaper class methodsFor: 'client freeze support'!

clientFunctionsOn: oo
	"String streamContents: [:oo | FeEdition clientConstantsOn: oo]"
	"Compute the client protocol."
	
	Heaper cachePromiseNameTableIn: 
	[self class selectors asSortedCollection do: 
		[:sel | 
		(self class selector: sel hasAttribute: 'CLIENT') ifTrue:
			[(self class selector: sel hasAttribute: 'constFn') ifTrue:
				[oo << 'constant ' << (ExtractMethodConstant from: (self class compiledMethodAt: sel)).
				oo << ' ' << (HxxPrintStream mapSelector: sel).
				oo cr]
			ifFalse:
				[| pat |
				pat _ Parser new parsePattern: (self class sourceCodeStringAt: sel).
				oo << 'function ' << NextClientRequestNumber << ' '.
				NextClientRequestNumber _ NextClientRequestNumber + 1.
				"Return type."
				oo << (self parseExportName: (pat at: 3)) << ' '.
				oo << (HxxPrintStream stripName: sel) << $(.
				(pat at: 2) 
					do: [:arg | 
						oo << (self parseExportName: arg type) << ' ' << arg variable name]
					andBetweenDo: [oo << ', '].
				oo << $).
				(self class selector: sel hasAttribute: 'login') ifTrue: [oo << ' login'].
				oo cr]]]]!

clientMethodsOn: oo
	"Text streamContents: [:oo | IntegerSpace clientMethodsOn: oo]"
	"Compute the client protocol."
	
	Heaper cachePromiseNameTableIn: 
		[self selectors asSortedCollection do: 
			[:sel | 
			(self selector: sel hasAttribute: 'CLIENT') ifTrue:
				[| pat |
				pat _ Parser new parsePattern: (self sourceCodeStringAt: sel).
				oo << 'message ' << NextClientRequestNumber << ' '.
				NextClientRequestNumber _ NextClientRequestNumber + 1.
				"Return type."
				oo << (self parseExportName: (pat at: 3)) << ' '.
				sel == #at:storeValue: ifTrue: [oo << 'store']
				ifFalse: [sel == #getValue: ifTrue: [oo << 'get']
				ifFalse: [oo << (HxxPrintStream stripName: sel)]].
				oo << $(.
				(pat at: 2) 
					do: [:arg | 
						oo << (self parseExportName: arg type) << ' ' << arg variable name]
					andBetweenDo: [oo << ', '].
				oo << $).
				(self selector: sel hasAttribute: 'login') ifTrue: [oo << ' login'].
				oo cr]]]!

clientProtocolOn: oo
	"String streamContents: [:oo | FeStatusDetector clientProtocolOn: oo]"
	"Compute the client protocol."
	
	self clientProtocolOn: oo with: 0!

clientProtocolOn: oo with: classNumber
	"String streamContents: [:oo | FeStatusDetector clientProtocolOn: oo with: 6]"
	"Compute the client protocol."
	
	Heaper cachePromiseNameTableIn: 
		[oo << 'class ' << classNumber.
		oo space << self exportName.
		self == Heaper 
			ifTrue: [oo << ' Root']
			ifFalse: [oo space << self superclass exportName].
		(self class includesSelector: #implicitReceiver) ifTrue: [oo << ' implicit'].
		oo << ' {'; cr.
		self clientFunctionsOn: oo.
		self clientMethodsOn: oo.
		oo << $}; cr]!

frozenClasses
	"Heaper frozenClasses."
	
	| clients stream  |
	clients _ OrderedCollection new.
	stream _ (Heaper getInfo: #promiseClasses) readStream.
	[stream atEnd] whileFalse:
		[clients addLast: (stream upTo: Character space)].
	^clients! !

!Heaper class methodsFor: 'locking'!

compileAllLocks
	"Heaper compileAllLocks"
	"Compile everything necessary to change instances to locked or unlocked versions of themselves."

	Transcript cr << 'Locks are now obsolete, and so were not generated.'.
	"Abraham withAllSubclasses do: [:cl | cl compileLocks]"!

{Class} compileLockingClass
	"Make the class for a version of myself that locks first, then runs methods.  We'll use
	 changeClassToThatOf: to switch instances between self and the polishClass."
	 
	| unlockedClass {Class} unlockedName {Symbol} type {String} lockedP {String} unlockedP {String} |
	unlockedName _ ((self name copyUpTo: $.) , 'Unlocked') asSymbol.
	lockedP _ 'Proto', self name.
	unlockedP _ 'Proto', unlockedName.
	unlockedClass _ self subclass: unlockedName
		instanceVariableNames: ''
		classVariableNames: lockedP, ' ' , unlockedP
		poolDictionaries: ''
		category: 'Unlocked Classes'.
	type _ '{', self name, '} '.

	unlockedClass compileGeneratedClassMethod:
'{BooleanVar} isGenerated ^true'.

	unlockedClass compileGeneratedMethod:
'{void} lock
	self changeClassToThatOf: self class lockedInstance'.

	unlockedClass compileGeneratedMethod:
'{void} unlock'.

	unlockedClass compileGeneratedMethod:
'{void} isUnlocked
	^true'.

	unlockedClass compileGeneratedClassMethod:
type , 'lockedInstance
	^', lockedP.

	unlockedClass compileGeneratedClassMethod:
type , 'unlockedInstance
	^', unlockedP.

	unlockedClass compileGeneratedClassMethod:
'{void} linkTimeNonInherited
', lockedP, ' _ ', self name, ' new.
', unlockedP, ' _ ', unlockedName, ' new'.

	unlockedClass compileGeneratedClassMethod:
'{void} cleanupGarbage
', lockedP, ' _ nil.
', unlockedP, ' _ nil'.

	^unlockedClass!

compileLockingMethods: unlockedClass {Class} 
	"compile stub methods for all selectors in unlockedClass sections"
	self withAllSuperclasses reverseDo:
		[ :class | 
		class ~= Object ifTrue:
		[class organization categories do: [ :category |
		((self protocol: category hasAttribute: #smalltalk) 
			or: [(self protocol: category hasAttribute: #generated) 
			or: [(self protocol: category hasAttribute: #protected) 
			or: [self protocol: category hasAttribute: #private]]]) 
			ifFalse:
		[(class organization listAtCategoryNamed: category) do:
			[ :selector |
			((unlockedClass includesSelector: selector)
				or: [(#(destruct changeClassToThatOf:) includes: selector)
					or: [class selector: selector hasAttribute: #NOLOCK]]) ifFalse:
			[| tree {ParseNode} |
			((class compiledMethodAt: selector) messages isEmpty
				or: [
					tree _ Parser new 
						parse: (class sourceCodeStringAt: selector) 
						class: class 
						notifying: nil.
					tree returnType == nil]) not ifTrue:
				[| sourceStream {Stream} hasReturn {BooleanVar} |
				sourceStream _ ReadWriteStream on: (String new: 100).
				tree printPatternOn: sourceStream.
				hasReturn _ tree returnsVoid not.
				sourceStream crtab.
				hasReturn ifTrue: [sourceStream << '| result '; << tree returnType; << ' |'; crtab].
				sourceStream << 'self lock.'; crtab.
				hasReturn ifTrue: [sourceStream << 'result _ '].
				sourceStream << 'super'.
				selector precedence = 1 
					ifTrue: [sourceStream space << selector]
					ifFalse: 
						[selector keywords with: tree block arguments do: 
							[:s :arg | 
							sourceStream space; << s; space; << arg name.
							"arg printOn: sourceStream indent: 0"]].
				sourceStream << $.; crtab;
					<< 'Abraham unlockFunctionAvoidingDestroy: self.'; crtab.
				hasReturn ifTrue: [sourceStream << '^result'].
				unlockedClass compile: sourceStream contents
					classified: category
					notifying: nil]]]]]]].!

compileLocks
	"Compile everything necessary to change instances to locked or unlocked versions of themselves."

	('*Unlocked' match: self name) not ifTrue:
		[ChangeLog suppressLoggingIn:
			[| lockedClass {Class} |
			(self getCxxClassDescription includesAttribute: #DEFERRED)
				ifFalse:
					[lockedClass _ self compileLockingClass.
					self compileLockingMethods: lockedClass.
					self compileUnlockingMethods: lockedClass]
				ifTrue:
					[(self getCxxClassDescription includesAttribute: #DEFERRED.LOCKED)
						ifFalse:
							[self warn: 'DEFERRED classes below a LOCKED class must be DEFERRED.LOCKED'.
							self getCxxClassDescription addAttribute: #DEFERRED.LOCKED]]]]!

compileSubclassLocks
	"compile subclass methods on me and all subclasses"
	"Abraham compileSubclassLocks"
	self withAllSubclasses do: [ :class |
		class compileLocks]!

{Class} compileUnlockedClass
	"Make the class for a version of myself that locks first, then runs methods.  We'll use
	 changeClassToThatOf: to switch instances between self and the polishClass."
	 
	| unlockedClass {Class} unlockedName {Symbol} type {String} lockedP {String} unlockedP {String} |
	unlockedName _ ((self name copyUpTo: $.) , 'Unlocked') asSymbol.
	lockedP _ 'Proto', self name.
	unlockedP _ 'Proto', unlockedName.
	unlockedClass _ self subclass: unlockedName
		instanceVariableNames: ''
		classVariableNames: lockedP, ' ' , unlockedP
		poolDictionaries: ''
		category: 'Unlocked Classes'.
	type _ '{', self name, '} '.

	unlockedClass compileGeneratedClassMethod:
 '{BooleanVar} isGenerated ^true'.
 
	unlockedClass compileGeneratedMethod:
 '{void} lock
	self changeClassToThatOf: self class lockedInstance'.

	unlockedClass compileGeneratedMethod:
 '{void} unlock'.
 
	unlockedClass compileGeneratedMethod:
 '{void} isUnlocked
	^true'.

	unlockedClass compileGeneratedClassMethod:
 type , 'lockedInstance
	^', lockedP.
 
	unlockedClass compileGeneratedClassMethod:
 type , 'unlockedInstance
	^', unlockedP.
 
	unlockedClass compileGeneratedClassMethod:
'{void} linkTimeNonInherited
	', lockedP, ' _ ', self name, ' new.
	', unlockedP, ' _ ', unlockedName, ' new'.

	unlockedClass compileGeneratedClassMethod:
'{void} cleanupGarbage
	', lockedP, ' _ nil.
	', unlockedP, ' _ nil'.

	^unlockedClass!

{Class} compileUnlockingMethods: polished
	"Add all the methods to the receiver to convert from tarnished (locked) to polished (unlocked)"

	((self getCxxClassDescription includesAttribute: #LOCKED) or: [self == Pumpkin])
		ifFalse:
			[self halt: 'concrete classes below a LOCKED class must be LOCKED'.
			"self getCxxClassDescription addAttribute: #LOCKED"].

"	self compileGeneratedMethod:
'{void} lock'.		"

	self compileGeneratedMethod:
'{void} unlock
	self changeClassToThatOf: ', polished name, ' unlockedInstance'.

"	self compileGeneratedMethod:
'{void} isUnlocked
	^false'.		"!

removeAllLocks
	"Heaper removeAllLocks."

	"Remove all lock methods and unlocked classes."

	Abraham allSubclassesDo: [:cl | cl removeLocks]!

removeLocks
	"Heaper removeAllLocking."

	ChangeLog suppressLoggingIn:
		[(self superclass name , 'Unlocked' = self name)
			ifTrue: [self removeFromSystem]
			ifFalse: 
				[self removeCategory: #'smalltalk: locking'.
				self removeCategory: #'smalltalk locking']]!

removeSubclassLocks
	"remove subclass methods on me and all subclasses"
	"Heaper removeSubclassLocks"
	self withAllSubclasses do: [ :class |
		class removeLocks]! !

!Heaper class methodsFor: 'smalltalk: initialization'!

cleanupGarbage
	"Heaper cleanupGarbage"
	GarbageCount _ IntegerVar0.
	InitializedClasses _ NULL.
	AllBlasts := nil.
	PackageTable _ NULL.
	LastMemory _ NULL!

enum: enumName {Symbol} flags: enums {Array of: Symbol | (Array of: Symbol)}
	"Define a bunch of single- or multi-bit flags.
	The first symbol is the name (or list of names) of a zero value of the enum type.
	Each subsequent element in the outer array takes up its own set of bit positions in the enum. If there is just a Symbol, it is a single bit. If there is an array of Symbols, the first one is an alternate name for zero for that set of bits, the next is ...1..., the next is ...01..., then ...11..., then ...100..., etc. In this case, the whole set takes up ceiling (log base 2 of the number of Symbols  in the subarray.)"
	
	| bit |
	bit := 0.
	enums do: [ :each |
		(each isKindOf: Symbol) ifTrue:
			[Smalltalk safeAt: each put: bit.
			bit := bit > 0 ifTrue: [bit * 2] ifFalse: [1]]
		ifFalse:
			[1 to: each size do: [ :i |
				Smalltalk safeAt: (each at: i) put: (i - 1) * bit].
			bit := bit > 0
				ifTrue: [bit bitShift: (each size ceilingLog: 2)]
				ifFalse: [1]]]!

enum: enumName {Symbol} with: enums {Array of: Symbol | (Array of: Symbol and: Integer)}
	"Define a bunch of enums. They start at 1 unless otherwise specified."
	
	| value |
	value := 0.
	enums do: [ :each |
		(each isKindOf: Symbol) ifTrue:
			[value := value + 1.
			Smalltalk safeAt: each put: value]
		ifFalse:
			[Smalltalk safeAt: (each at: 1) put: (value := each at: 2)]]!

initializedClasses
	^InitializedClasses!

initializingClasses
	^InitializingClasses!

initPackages

	myPackageClasses _ nil!

initStringHashSBoxes
	"Heaper initStringHashSBoxes"
	| rs sboxes |
	rs _ RandomStepper make: 23.  "If this is changed, it must also be in X++"
	sboxes _ PtrArray nulls: 8.
	0 to: 7 do: [ :i | | sbox |
		sboxes at: i put: (sbox _ PrimIntArray zeros: 32 with: 256).
		0 to: 255 do: [ :j |
			sbox at: j put: rs value.
			rs step]].
	StringHashSBoxes _ sboxes!

linkTimeNonInherited
	MACINTOSH _ XVIEW _ false.
	GarbageCount _ IntegerVar0.
	InGC _ false.
	BecomeMap _ IdentityDictionary new.
	MAX.U.CATEGORY.U.NAME _ 4088.
	PackageTable _ IdentityDictionary new.
	LastMemory _ NULL.
	self computePreorder: 0.! !

!Heaper class methodsFor: 'global: accessing'!

{void} canYouBecome: cat {Category} 

	| set {MuSet} |
	((self isEqual: cat) or: [(self isEqual: SuspendedHeaper) or: [cat isEqual: SuspendedHeaper]])
		ifTrue: [^true].
	set _ BecomeMap at: self ifAbsent: [NULL].
	^(set ~~ NULL and: [set includes: cat]) or: [self ~~ Heaper and: [self superclass canYouBecome: cat]]!

{BooleanVar} compare: a {Heaper | NULL} with: b {Heaper | NULL}
	^a == b or: [a ~~ NULL and: [b ~~ NULL and: [a isEqual: b]]]!

fetchSuperCategory
	self == Heaper ifTrue: [^NULL] ifFalse: [^self superclass]!

{Class} findCategory: catName {UInt8Array}
	"translate a category name into a class"
	"Warning:  smalltalk wizardy."
	| oldCat result |
	catName isSymbol 
		ifTrue: [^Smalltalk at: catName
			ifAbsent: [Heaper BLAST: #'CATEGORY_NOT_FOUND']].
	oldCat _ catName class.
	catName changeClassToThatOf: ''.
	result _ Smalltalk at: catName asSymbol 
		ifAbsent: [Heaper BLAST: #'CATEGORY_NOT_FOUND'].
	catName changeClassToThatOf: oldCat basicNew.
	^result!

getSuperCategory
	(self == Heaper) not assert.
	^superclass!

{Uint3} instanceSize
	^self instSize * 4!

{Boolean} isEqualOrSubclassOf: cat {Category}

	^ self == cat or: [self inheritsFrom: cat]!

{void} mayBecome: cat {Category} 
	| set {MuSet of: Category} |
	set _ BecomeMap at: self ifAbsent: 
		[set _ Set new.
		BecomeMap at: self put: set].
	set add: cat!

{void} mayBecomeAnySubclassOf: cat {Category} 
	| set {MuSet} |
	set _ BecomeMap at: self ifAbsent: 
		[set _ Set new.
		BecomeMap at: self put: set].
	cat withAllSubclasses do: [:sub | set add: sub]! !

!Heaper class methodsFor: 'smalltalk: creation'!

create
	"Create the frame and call the constructor defined for instances."
	^self new create!

create.IntegerVar: arg1
	"Create the frame and call the constructor defined for instances."

	^self new create.IntegerVar: arg1!

{void} create.Rcvr: rcvr {Rcvr}
	^self new create.Rcvr: rcvr!

create: arg1
	"Create the frame and call the constructor defined for instances."

	^self new create: arg1!

create: arg1 with: arg2
	"Create the frame and call the constructor defined for instances."

	^self new create: arg1 with: arg2!

create: arg1 with: arg2 with: arg3
	"Create the frame and call the constructor defined for instances."

	^self new create: arg1 with: arg2 with: arg3!

create: arg1 with: arg2 with: arg3 with: arg4
	"Create the frame and call the constructor defined for instances."

	^self new create: arg1 with: arg2 with: arg3 with: arg4!

create: arg1 with: arg2 with: arg3 with: arg4 with: arg5
	"Create the frame and call the constructor defined for instances."

	^self new create: arg1 with: arg2 with: arg3 with: arg4 with: arg5!

create: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6
	"Create the frame and call the constructor defined for instances."

	^self new create: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6!

create: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6 with: arg7
	"Create the frame and call the constructor defined for instances."

	^self new create: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6 with: arg7!

create: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6 with: arg7 with: arg8
	"Create the frame and call the constructor defined for instances."

	^self new create: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6 with: arg7 with: arg8!

new.AllocType: allocType {Symbol} 
	"This does nothing in smalltalk, but allows translated code to specify 
	how a new object is allocated. Returns self since Heaper creates do 
	^self new create..."

	^self!

{Heaper} new.Become: memory {Heaper | DeletedHeaper} 
	"Create the frame for a new instance using the identity of memory. This 
	corresponds to creating the frame in the actual memory in c++. First call the 
	destructor for memory."

	memory class ~~ DeletedHeaper ifTrue:   "This takes care of the void star case."
		[(memory getCategory canYouBecome: self) assert: 'Can only become something that was explicitly allowed.'.
		memory destructor].
	(LastMemory _ self new) become: memory.
	^memory! !

!Heaper class methodsFor: 'global: blasts'!

BLAST: problem {Symbol}
	(self signal: problem) raise!

blast: problem {Problem}
	"raise an exception"
	
	(Heaper signal: problem getProblemName asSymbol) raiseWith: problem!

PROBLEM: exception

	^exception signal!

problems.AllBlasts
	"A ProblemList for all X++ BLASTs"
	
	XppBlast == nil ifTrue:
			[XppBlast := Signal new nameClass: 'BLAST' message: #All; notifierString: 'BLAST(!!!!)'].
	^XppBlast!

{Signal} signal: signal {Symbol}
	"Define a blast and return a unique and persistent signal for it."
	
	AllBlasts == nil ifTrue:
		[AllBlasts := IdentityDictionary new].
	^AllBlasts at: signal
		ifAbsent:
			[AllBlasts at: signal
				put: (self problems.AllBlasts newSignal nameClass: 'BLAST' message: signal;
					notifierString: 'BLAST(' , signal , ')';
					yourself)]!

{Signal} signals: signals {Array of: Symbol}
	"Define a set of blasts blast and return a SignalCollection."
	
	| result |
	signals size = 1 ifTrue:
		[^self signal: signals first].
	result := SignalCollection new.
	signals do: [ :each |
		result add: (self signal: each)].
	^result! !

!Heaper class methodsFor: 'hooks: global: xpp hooks'!

IntegerVar: x
	^x!

{void} scheduleTermination
	"In xpp, smainx.cxx defines this to set a flag requesting server termination after current request."
	self thingToDo.
	"This should do something in smalltalk?"! !

!Heaper class methodsFor: 'global: garbage collection'!

collectibleClasses
	"Return the list of classes whose instances need to be garbage
	 collected.  I don't know whether this needs to return the locked 
	 or unlocked versions of classes." 
 
	| acc |
	acc _ TableAccumulator make.
	"acc step: ActualHand."
	"acc step: PhantomHand."
	acc step: IDPair.
	acc step: ActualXID.
	acc step: ActualOrgl.
	acc step: HRootUnlocked.
			acc step: StampUnlocked.
			acc step: BertUnlocked.
			acc step: ActualOrglRootUnlocked.
			acc step: EmptyOrglRootUnlocked.
			acc step: RegionLoafUnlocked.
			acc step: OVirtualLoafUnlocked.
			acc step: SplitLoafUnlocked.
			acc step: DspLoafUnlocked.
			acc step: OPartialLoafUnlocked.
			"acc step: HBottomCrumUnlocked."
			"acc step: HUpperCrumUnlocked."
			acc step: SensorCrumUnlocked.
			acc step: BertCrumUnlocked.
	^acc value!

garbageCollect
	| roots |
	roots _ ActualHand allInstances.
	roots add: CurrentGrandMap fluidFetch.
	roots addAll: PhantomHand allInstances.
	self garbageCollectFrom: roots!

garbageCollectFrom: roots
	"This is an attept to simulate the X++ garbage collector. 
	 Only instances of the classes returned by Heaper>collectibleClasses 
	 will actually get deleted.  Further, this assumes that it gets run 
	 after all operations are finished because it doesn't mark instances 
	 from objects on the stack (it could be made to...)."

	| counts garbage | 
	counts _ IdentityDictionary new.
	GarbageCount _ GarbageCount + 1.
	InGC _ true.
	Transcript show: 'Start marking...'; cr.
	roots stepper forEach: [:root {Heaper} | root markInstances: GarbageCount].
	Transcript show: 'Start collecting...'; cr.
	garbage _ IdentitySet new: 120.
	self collectibleClasses stepper forEach: 
		[:class || count |
		count _ 0.
		class allInstances do: 
			[:heaper |
			(heaper markCount ~~ nil 
				and: [heaper markCount < GarbageCount])
				ifTrue:
					[count _ count + 1.
					((heaper isKindOf: Abraham) and: [heaper isForgotten not])
						ifTrue: [garbage add: heaper]
						ifFalse: [heaper destruct; delete]].
			count ~~ 0 ifTrue: [counts at: class put: count]]].
	Transcript show: 'garbage size: ', garbage size printString; cr.
	[garbage isEmpty] whileFalse: 
		[| oldGarbage |
		oldGarbage _ garbage.
		garbage _ IdentitySet new: 120.
		oldGarbage do: [:heaper |
			heaper isForgotten
				ifTrue: 
					[| pos |
					pos _ heaper class.
					heaper destruct; delete.
					counts at: pos put: (counts at: pos ifAbsent: [0]) + 1]
				ifFalse: [garbage add: heaper]].
		garbage size == oldGarbage size 
			ifTrue: 
				[Transcript show: 'All remaining shepherds are not forgotten.'.
				self halt]].
	InGC _ false.
	Transcript show: 'Done.  Collected: ',  counts printString; cr!

{void} gcOpportunity
	"A no-op method in smalltalk to tell c++ it is a good time to garbage collect."!

{void} gcOpportunity: anInt
	"A no-op method in smalltalk to tell c++ it is a good time to garbage collect."
	"The arguments for now are -1 means do it now, n>0 means on the n-th alloc,
	otherwise it's up to the receiver."!

{BooleanVar} inGC
	"Some destructors need to have different behavior during garbage 
	collection, since some of their instance variables may already have 
	been collected. In X++, this will return true during garbage 
	collection."

	^InGC!

{BooleanVar} isConstructed: obj {void star}
	^obj ~~ NULL and: [obj class ~~ DeletedHeaper]!

{BooleanVar} isDestructed: obj {void star}
	^obj == NULL or: [obj class == DeletedHeaper]!

{void} setGC: flag {BooleanVar}
	"Some destructors need to have different behavior during garbage 
	collection, since some of their instance variables may already have 
	been collected. In X++, this will return true during garbage 
	collection."

	^InGC _ flag! !

!Heaper class methodsFor: 'global: packages'!

{void} addPackageCategory: packageCat {PackageCategory}
	"add a package category to my list"
	self packageClasses add: packageCat!

{Symbol} fetchAttribute: attr
	"get information attached to the given attribute, or NULL if none"
	^self getCxxClassDescription fetchAttribute: attr!

inspectPieces
	^self subclasses asOrderedCollection , self packageClasses!

{OrderedCollection of: Category} packageClasses
	"return my packages classes and create if needed"
	(myPackageClasses == nil) ifTrue:
		[myPackageClasses _ OrderedCollection new].
	^myPackageClasses!

{Category} registerPackageCategory: packageCat {PackageCategory}
	"a package category can be added; remember it and return what its contents class should be"
	(self hasAttribute: #HOOK)
		ifTrue: [^(FakeCategory create: packageCat name , '_' , self name
				with: self
				with: (packageCat getSuperCategory == Package
					ifTrue: [self getSuperCategory]
					ifFalse: [packageCat getSuperCategory contentsCategory]))
			addPackageCategory: packageCat;
			yourself]
		ifFalse: [self addPackageCategory: packageCat.
			^self]! !

!Heaper class methodsFor: 'stubble tools'!

{String} abstractDeclarationFor: aSymbol {Symbol}

	| cat | 
	aSymbol == #void ifTrue: [^'{void}'].
	aSymbol == #BooleanVar ifTrue: [^'{BooleanVar}'].
	aSymbol == #IntegerVar ifTrue: [^'{IntegerVar}'].
	aSymbol == #IEEEDoubleVar ifTrue: [^'{IEEEDoubleVar}'].
	aSymbol == #SnarfID ifTrue: [^'{Int32}'].
	
	aSymbol == #Int32 ifTrue: [^'{Int32}'].
	aSymbol == #UInt32 ifTrue: [^'{UInt32}'].
	aSymbol == #Int8 ifTrue: [^'{Int8}'].
	aSymbol == #UInt8 ifTrue: [^'{UInt8}'].
	
	aSymbol == #char ifTrue: [^'{char star}'].
	cat _ Smalltalk at: aSymbol
		ifAbsent: [self warn: 'unrecognized type encountered in Heaper>>abstractDeclarationFor:'].
	(cat isBehavior and: [(cat inheritsFrom: Heaper) or: [cat inheritsFrom: PrimArray]])
		ifTrue: [^'{Heaper}'].
	(cat == Heaper) ifTrue: [^'{Heaper}']. 
	self halt: 'unrecognized type encountered'.!

{String} abstractTypeFor: aSymbol {Symbol} 
	| cat |
	aSymbol == #void ifTrue: [^'Void'].
	aSymbol == #BooleanVar ifTrue: [^'BooleanVar'].
	aSymbol == #IntegerVar ifTrue: [^'IntegerVar']. 
	aSymbol == #SnarfID ifTrue: [^'Int32'].
	
	aSymbol == #Int32 ifTrue: [^'Int32'].
	aSymbol == #UInt32 ifTrue: [^'UInt32'].
	aSymbol == #Int8 ifTrue: [^'Int8'].
	aSymbol == #UInt8 ifTrue: [^'UInt8']. 
	
	aSymbol == #char ifTrue: [^'String'].
	aSymbol == #IEEEDoubleVar ifTrue: [^'IEEEDoubleVar'].
	cat _ Smalltalk at: aSymbol ifAbsent: [self warn: 'unrecognized type: ', aSymbol, ' in: ', self name.  ^'Int32'].
	(cat inheritsFrom: PrimArray) ifTrue: [^'Heaper'].
	(cat isBehavior and: [cat inheritsFrom: Heaper]) ifTrue: [^'Heaper'].
	cat == Heaper ifTrue: [^'Heaper'].
	self warn: 'unrecognized type: ', aSymbol, ' in: ', self name.
	^'Heaper'!

copyReferencesToType: symb {Symbol}
	"Return the list of variables in this class that is non-copy."
	"Abraham copyReferencesToType: #MuTable"
	
	| nocopy |
	nocopy _ IdentityDictionary new.
	self withAllSubclasses do: 
		[:class |
		| cxx {CxxClassDescription} tab |
		cxx _ class fetchCxxClassDescription.
		tab _ Dictionary new.
		cxx ~~ nil ifTrue:
			[cxx parsedInstVarTypes do: [:node {ParameterNode} |
				| type |
				((type _ TypeDescription fromParse: node type with: class) isNoCopy not and: [type baseType = symb])
					ifTrue: [tab at: node name put: type]]].
		tab isEmpty ifFalse: [nocopy at: class name put: tab]].
	^nocopy!

nonCopyVariables
	"Return the list of variables in this class that is non-copy."
	"GrandMap nonCopyVariables"
	
	| cxx {CxxClassDescription} nocopy {Collection of: Association} |
	cxx _ self fetchCxxClassDescription.
	cxx == nil ifTrue: [^Set new].
	nocopy _ Set new.
	cxx parsedInstVarTypes do: [:node {ParameterNode} | 
		(TypeDescription fromParse: node type with: self) isNoCopy 
			ifFalse: [nocopy add: node name]].
	^nocopy!

subclassNonCopyVariables
	"Return the list of variables in this class that is non-copy."
	"Heaper subclassNonCopyVariables"
	
	| nocopy |
	nocopy _ IdentityDictionary new.
	self withAllSubclasses do: 
		[:class |
		| set |
		set _ class nonCopyVariables.
		set isEmpty ifFalse: [nocopy at: class name put: set]].
	^nocopy! !

!Heaper class methodsFor: 'smalltalk: passe'!

constant: constant {Symbol} type: type {String} value: value {Heaper}
	"define a global constant"
	"Ignore for the moment because otherwise initialization will fail."!

makeFillTable: filename
	self passe. "use makeRequestTable:"! !

!Heaper class methodsFor: 'global: preorder'!

computePreorder: mine
	"Set my preorder number and pass it on down recursively to my subclasses.  Return the next unused preorder number."
	"Heaper computePreorder: 0."
	
	| preorder {Int32} |
	myPreorderNumber _ mine.
	preorder _ mine+1.
	(self subclasses asSortedCollection: [:a :b | a name <= b name])
		do: [:cls | preorder _ cls computePreorder: preorder].
	myPreorderMax _ preorder - 1.
	^preorder!

preorderMax
	^myPreorderMax!

preorderNumber
	^myPreorderNumber! !

!Heaper class methodsFor: 'function pointers'!

pointerToStaticMember: selector {Symbol}

	^StaticFunctionPointer make: self with: selector!

pointerToStaticMember: selector {Symbol} with: typeString {String}

	^self pointerToStaticMember: selector!

pointerToVirtualMember: selector {Symbol}

	^selector! !

!Heaper class methodsFor: 'smalltalk: system'!

info.clientClasses
"XuHeaper XuAdminer XuArchiver XuBundle XuArrayBundle XuElementBundle XuPlaceHolderBundle XuCalc XuCalcCreator XuCoordinateSpace XuCrossSpace XuFilterSpace XuIDSpace XuIntegerSpace XuRealSpace XuSequenceSpace XuFillRangeDetector XuFillDetector XuGateKeeper XuServer XuKeyMaster XuLock XuBooLock XuChallengeLock XuMatchLock XuMultiLock XuWallLock XuMapping XuCrossMapping XuIntegerMapping XuSequenceMapping XuOrderSpec XuCrossOrderSpec XuPopSensor XuPosition XuFilterPosition XuID XuInteger XuReal XuSequence XuTuple XuPrimArray XuPrimSpec XuPrimFloatSpec XuPrimIntegerSpec XuPrimPointerSpec XuPrimValue XuPrimFloat XuPrimInteger XuRangeElement XuDataHolder XuEdition XuIDHolder XuLabel XuWork XuClub XuRational XuRegion XuCrossRegion XuFilter XuIDRegion XuIntegerRegion XuRealRegion XuSequenceRegion XuRevisionDetector XuSession XuStatusDetector XuStepper XuTableStepper XuWaitDetector XuWrapper XuClubDescription XuHyperLink XuHyperRef XuMultiRef XuSingleRef XuLockSmith XuBooLockSmith XuChallengeLockSmith XuMatchLockSmith XuMultiLockSmith XuWallLockSmith XuPath XuSet XuText XuWrapperSpec "!

info.clientSideClasses
"FillRangeDetector FillDetector RevisionDetector StatusDetector WaitDetector Wrapper"!

info.promiseClasses
"Promise Adminer Archiver Array FloatArray HumberArray IntArray PtrArray Bundle ArrayBundle ElementBundle PlaceHolderBundle CoordinateSpace CrossSpace FilterSpace IDSpace IntegerSpace RealSpace SequenceSpace FillRangeDetector FillDetector KeyMaster Lock BooLock ChallengeLock MatchLock MultiLock WallLock Mapping CrossMapping IntegerMapping SequenceMapping OrderSpec CrossOrderSpec Position FilterPosition ID Sequence Tuple Integer Real RangeElement DataHolder Edition IDHolder Label Work Club RevisionDetector Server Session StatusDetector Stepper TableStepper WaitDetector Wrapper ClubDescription HyperLink HyperRef MultiRef SingleRef LockSmith BooLockSmith ChallengeLockSmith MatchLockSmith MultiLockSmith WallLockSmith Path Set Text WrapperSpec Region CrossRegion Filter IDRegion IntegerRegion RealRegion SequenceRegion Value FloatValue IntValue"!

info.stProtocol
"{void NOLOCK CLIENT} destroy
{UInt32 CLIENT} actualHashForEqual
{BooleanVar CLIENT} isEqual: other {Heaper}
"! !

!Heaper class methodsFor: 'accessing'!

{UInt32} takeOop
	^ self asOop! !

Heaper subclass: #CalcCreator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Basic'!

(CalcCreator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #BY.PROXY; add: #DEFERRED; add: #EQ; yourself)!

!CalcCreator methodsFor: 'proxy accessing'!

{Calc PROXY} newCalc
	"return a new calculator"

	self subclassResponsibility! !

!CalcCreator methodsFor: 'generated:'!

actualHashForEqual ^self asOop!

isEqual: other ^self == other! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

CalcCreator class
	instanceVariableNames: ''!

(CalcCreator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #BY.PROXY; add: #DEFERRED; add: #EQ; yourself)!

!CalcCreator class methodsFor: 'smalltalk: init'!

cleanupGarbage
	CalcCuisine _ NULL!

initTimeNonInherited
	Cookbook declareCookbook: 'calc' with: CalcCreator with: CalcCuisine with: XppCuisine!

linkTimeNonInherited
	Recipe star defineGlobal: #CalcCuisine with: NULL.! !

CalcCreator subclass: #ActualCalcCreator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Basic'!

(ActualCalcCreator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!

!ActualCalcCreator methodsFor: 'accessing'!

{Calc PROXY} newCalc
	"return a new calculator"

	^ActualCalc create: 20! !

!ActualCalcCreator methodsFor: 'generated:'!

actualHashForEqual ^self asOop!

isEqual: other ^self == other! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

ActualCalcCreator class
	instanceVariableNames: ''!

(ActualCalcCreator getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!

!ActualCalcCreator class methodsFor: 'pseudo-constructor'!

{CalcCreator} make
	^self create! !

Heaper subclass: #Category
	instanceVariableNames: 'myClass {UNKNOWN}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xpp-Basic'!



!Category methodsFor: 'accessing'!

brotherClass
	^myClass!

create: cat 
	super create.
	myClass _ cat!

getCategory
	^Category create: Category!

inheritsFrom: cat
	cat class == Category ifTrue: [^myClass == cat brotherClass or: [myClass inheritsFrom: cat brotherClass]]
		ifFalse: [^myClass == cat or: [myClass inheritsFrom: cat]]!

name
	^myClass name!

preorderNumber
	^myClass preorderNumber! !

!Category methodsFor: 'stubble'!

sendSelfTo: xmtr
	xmtr sendString: self name! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Category class
	instanceVariableNames: ''!



!Category class methodsFor: 'stubble'!

create.Rcvr: rcvr
	| catName |
	catName _ rcvr receiveString.
	^Heaper findCategory: catName! !

!Category class methodsFor: 'accessing'!

{Class} find: catName {UInt8Array}
	"translate a category name into a class"
	"Warning:  smalltalk wizardy."
	| oldCat result |
	catName isSymbol 
		ifTrue: [^Smalltalk at: catName
			ifAbsent: [Heaper BLAST: #'CATEGORY_NOT_FOUND']].
	oldCat _ catName class.
	catName changeClassToThatOf: ''.
	result _ Smalltalk at: catName asSymbol 
		ifAbsent: [Heaper BLAST: #'CATEGORY_NOT_FOUND'].
	catName changeClassToThatOf: oldCat basicNew.
	^result! !