/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xpp.basic;

import info.dgjones.abora.gold.java.AboraHeaper;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Dictionary;
import info.dgjones.abora.gold.java.missing.smalltalk.IdentityDictionary;
import info.dgjones.abora.gold.java.missing.smalltalk.OrderedCollection;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.DeletedHeaper;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * This is the base heap allocated class in X++.
 */
public class Heaper extends AboraHeaper {

	protected static Heaper AllBlasts;
	protected static Heaper BecomeMap;
	protected static Heaper GarbageCount;
	protected static Heaper InGC;
	protected static Set InitializedClasses;
	protected static Set InitializingClasses;
	protected static Heaper LastMemory;
	protected static int NextClientRequestNumber;
	protected static Heaper NotOneElementSignal;
	protected static Heaper PackageTable;
	protected static IdentityDictionary PromiseNameTable;
	protected static Heaper StringHashSBoxes;
	protected static Dictionary myTokens;
	protected static Dictionary mySelectors;
	protected static Dictionary myReturn;
	protected static Dictionary myArguments;
	protected static int myPreorderNumber;
	protected static int myPreorderMax;
	protected static OrderedCollection myPackageClasses;
/*
Xanadu-Xpp-Basic.st:187:
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
*/
/*
Xanadu-Xpp-Basic.st:205:
Heaper comment:
'This is the base heap allocated class in X++. '!
*/
/*
Xanadu-Xpp-Basic.st:207:
(Heaper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
Xanadu-Xpp-Basic.st:418:
Heaper class
	instanceVariableNames: 'myTokens {Dictionary smalltalk} mySelectors {Dictionary smalltalk} myReturn {Dictionary smalltalk} myArguments {Dictionary smalltalk} myPreorderNumber {IntegerVar} myPreorderMax {IntegerVar} myPackageClasses {OrderedCollection smalltalk of: Category} '!
*/
/*
Xanadu-Xpp-Basic.st:421:
(Heaper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; yourself)!
*/
/*
Xanadu-Xpp-Basic.st:778:Heaper class methodsFor: 'stubble PROXY'!
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
*/
/*
Xanadu-Xpp-Basic.st:790:
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
*/
/*
Xanadu-Xpp-Basic.st:826:Heaper class methodsFor: 'stubble PROXY'!
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
*/
/*
Xanadu-Xpp-Basic.st:860:Heaper class methodsFor: 'stubble PROXY'!
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
					self createRequestClass: return arguments: arguments.]]]]!
*/
/*
Xanadu-Xpp-Basic.st:1016:
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
	requestClass compileStubbleMethods!
*/
/*
Xanadu-Xpp-Basic.st:1149:
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
*/
/*
Xanadu-Xpp-Basic.st:1695:
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
*/
/*
Xanadu-Xpp-Basic.st:2101:Heaper class methodsFor: 'locking'!
compileAllLocks
	"Heaper compileAllLocks"
	"Compile everything necessary to change instances to locked or unlocked versions of themselves."
	Transcript cr << 'Locks are now obsolete, and so were not generated.'.
	"Abraham withAllSubclasses do: [:cl | cl compileLocks]"!
*/
/*
Xanadu-Xpp-Basic.st:2108:
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
*/
/*
Xanadu-Xpp-Basic.st:2157:Heaper class methodsFor: 'locking'!
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
*/
/*
Xanadu-Xpp-Basic.st:2204:Heaper class methodsFor: 'locking'!
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
*/
/*
Xanadu-Xpp-Basic.st:2221:Heaper class methodsFor: 'locking'!
compileSubclassLocks
	"compile subclass methods on me and all subclasses"
	"Abraham compileSubclassLocks"
	self withAllSubclasses do: [ :class |
		class compileLocks]!
*/
/*
Xanadu-Xpp-Basic.st:2227:
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
*/
/*
Xanadu-Xpp-Basic.st:2276:Heaper class methodsFor: 'locking'!
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
*/
/*
Xanadu-Xpp-Basic.st:2295:Heaper class methodsFor: 'locking'!
removeAllLocks
	"Heaper removeAllLocks."
	"Remove all lock methods and unlocked classes."
	Abraham allSubclassesDo: [:cl | cl removeLocks]!
*/
/*
Xanadu-Xpp-Basic.st:2302:Heaper class methodsFor: 'locking'!
removeLocks
	"Heaper removeAllLocking."
	ChangeLog suppressLoggingIn:
		[(self superclass name , 'Unlocked' = self name)
			ifTrue: [self removeFromSystem]
			ifFalse: 
				[self removeCategory: #'smalltalk: locking'.
				self removeCategory: #'smalltalk locking']]!
*/
/*
Xanadu-Xpp-Basic.st:2312:Heaper class methodsFor: 'locking'!
removeSubclassLocks
	"remove subclass methods on me and all subclasses"
	"Heaper removeSubclassLocks"
	self withAllSubclasses do: [ :class |
		class removeLocks]!
*/
/*

Generated during transformation: AddMethod
*/
/*
Xanadu-Xpp-Basic.st:212:Heaper methodsFor: 'anachronisms'!
notWorking
	self error: 'this method does not work...'!
*/
public static void passe() {
	throw new AboraRuntimeException("this routine is just passe...");
/*
Xanadu-Xpp-Basic.st:215:Heaper methodsFor: 'anachronisms'!
{void} passe
	self error: 'this routine is just passe...'!
*/
}
/*
Xanadu-Xpp-Basic.st:218:Heaper methodsFor: 'anachronisms'!
unimplemented
	Heaper BLAST: #'NOT_YET_IMPLEMENTED'!
*/
public void destroy() {
	destruct();
	destructor();
	delete();
/*
Xanadu-Xpp-Basic.st:224:Heaper methodsFor: 'create/destroy'!
{void NOLOCK} destroy
	self destruct.
	self destructor.
	self delete!
*/
}
/**
 * Classes should implement this message rather than a destructor. We use this so
 * the destruction behavior implemented in abstract superclasses can access the
 * vtable of the concrete run-time type in C++. Using a message makes C++
 * parallel the Smalltalk semantics for delete.  Destroy will actually call the destructor.
 */
public void destruct() {
/*
Xanadu-Xpp-Basic.st:229:Heaper methodsFor: 'create/destroy'!
{void NOLOCK} destruct
	"Classes should implement this message rather than a destructor. We use this so 
	the destruction behavior implemented in abstract superclasses can access the 
	vtable of the concrete run-time type in C++. Using a message makes C++ 
	parallel the Smalltalk semantics for delete.  Destroy will actually call the destructor."!
*/
}
/*
Xanadu-Xpp-Basic.st:237:Heaper methodsFor: 'xlate hack'!
arrow: aSymbol
	"This is for references to struct fields.  
		foo arrow: #field  
		translates to
		foo->field."
	self error: 'Not implemented in smalltalk; C++ only operation!!'.
	
	self arrow: #field!
*/
/*
Xanadu-Xpp-Basic.st:246:Heaper methodsFor: 'xlate hack'!
deref
	"This is so that PrimArray arguments can be used identically in smalltalk and X++"
	"Given UInt8Array * array,   array at: i ==> array[i] which is not right."
	"array deref at: i ==> (*(array))[i] which has the desired behavior."
	^ self!
*/
public Heaper(Rcvr rcvr) {
/*
Xanadu-Xpp-Basic.st:254:Heaper methodsFor: 'stubble'!
{void} create.Rcvr: rcvr {Rcvr}!
*/
}
/*
Xanadu-Xpp-Basic.st:256:Heaper methodsFor: 'stubble'!
{BooleanVar} isByProxy
	^false!
*/
/*
Xanadu-Xpp-Basic.st:259:Heaper methodsFor: 'stubble'!
{void} sendProxyTo: trans {Xmtr}
	
	self stubbleForSubclassResponsibility!
*/
/**
 * do nothing
 */
public void sendSelfTo(Xmtr trans) {
/*
Xanadu-Xpp-Basic.st:263:Heaper methodsFor: 'stubble'!
{void} sendSelfTo: trans {Xmtr}
	"do nothing"!
*/
}
/*
Xanadu-Xpp-Basic.st:268:Heaper methodsFor: 'converting'!
{Heaper} convert: type {Category}
	"do a type conversion and a cast"
	^Converter CONVERT: type with: self!
*/
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(????)");
/*
Xanadu-Xpp-Basic.st:274:Heaper methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(????)'.!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
Xanadu-Xpp-Basic.st:279:Heaper methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^ self asOop!
*/
}
public int hashForEqual() {
	return actualHashForEqual();
/*
Xanadu-Xpp-Basic.st:282:Heaper methodsFor: 'testing'!
{UInt32} hashForEqual
	^ self actualHashForEqual!
*/
}
/**
 * Return true if the two objects are equal.
 */
public boolean isEqual(Heaper other) {
	throw new SubclassResponsibilityException();
/*
Xanadu-Xpp-Basic.st:285:Heaper methodsFor: 'testing'!
{BooleanVar} isEqual: other {Heaper}
	"Return true if the two objects are equal."
	self subclassResponsibility!
*/
}
/*
Xanadu-Xpp-Basic.st:291:Heaper methodsFor: 'accessing'!
{Category} getCategory
	^ self class!
*/
/*
Xanadu-Xpp-Basic.st:296:Heaper methodsFor: 'twiddles'!
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
	^result!
*/
public Heaper() {
/*
Xanadu-Xpp-Basic.st:316:Heaper methodsFor: 'smalltalk: gc'!
create!
*/
}
/*
Xanadu-Xpp-Basic.st:318:Heaper methodsFor: 'smalltalk: gc'!
delete	
	self nilFields.!
*/
/*
Xanadu-Xpp-Basic.st:321:Heaper methodsFor: 'smalltalk: gc'!
{void} destructor
	"No other class should implement this.  This will eventually become
	 deleted objects in Smalltalk to Heaper."
	
	self become: DeletedHeaper create!
*/
/*
Xanadu-Xpp-Basic.st:327:Heaper methodsFor: 'smalltalk: gc'!
isUnlocked
	^true!
*/
/*
Xanadu-Xpp-Basic.st:330:Heaper methodsFor: 'smalltalk: gc'!
{void} markChildren: count {IntegerVar}
	"This ia a generic method to mark all of the pointers coming
	 out of an object.  Subclasses with lots of IntegerVars or with 
	 wimpy pointers should reimplement this."
	| class |
	class _ self class.
	1 to: class instSize do: [:i | (self instVarAt: i) markInstances: count]!
*/
/*
Xanadu-Xpp-Basic.st:339:Heaper methodsFor: 'smalltalk: gc'!
{IntegerVar} markCount
	"Return the count of the last garbage collection i which this object was marked."
	
	^-1!
*/
/*
Xanadu-Xpp-Basic.st:344:Heaper methodsFor: 'smalltalk: gc'!
{void} markInstances: count {IntegerVar} 
	"The count is so we don't need to clear the marks."!
*/
/*
Xanadu-Xpp-Basic.st:349:Heaper methodsFor: 'packages'!
{void} addPackage: package {Package}
	"add a package to the list"
	(PackageTable at: self ifAbsent: [PackageTable at: self put: (OrderedCollection with: package).  ^self]) add: package!
*/
/*
Xanadu-Xpp-Basic.st:354:Heaper methodsFor: 'packages'!
{Package} fetchPackage: cat {PackageCategory}
	"retrieve a package attached to this object"
	
	| packages {OrderedCollection} |
	packages _ PackageTable at: self ifAbsent: [^NULL].
	packages do: [ :p | (p packagingCategory isEqualOrSubclassOf: cat) ifTrue: [^p]].
	^NULL!
*/
/*
Xanadu-Xpp-Basic.st:362:Heaper methodsFor: 'packages'!
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
*/
/*
Xanadu-Xpp-Basic.st:376:Heaper methodsFor: 'packages'!
{Package} pack: packageCat {PackageCategory}
	"this gets translated into a macro PACK(PackageClassName,contents)"
	^self getOrMakePackage: packageCat!
*/
/*
Xanadu-Xpp-Basic.st:380:Heaper methodsFor: 'packages'!
{Category} packagingCategory
	"the category object to use for packaging purposes"
	^self class!
*/
/*
Xanadu-Xpp-Basic.st:386:Heaper methodsFor: 'smalltalk: passe'!
foo.IntegerVar: x
	self junk!
*/
/*
Xanadu-Xpp-Basic.st:392:Heaper methodsFor: 'exceptions: exceptions'!
blast: problemName {Symbol}
	"raise an exception"
	
	(Heaper signal: problemName) raise!
*/
/*
Xanadu-Xpp-Basic.st:399:Heaper methodsFor: 'promise messages'!
{Heaper CLIENT login} cast.IntegerVar: catNum {IntegerVar}
	"Return the object if it is of the appropriate type."
	"Note that cast and isKindOf could be implemented in terms of each other is there were operations to commute between booleans and broken promises."
	
	Dean shouldImplement!
*/
/**
 * Return true if the two objects are equal.
 */
public boolean equals(Heaper other) {
	return isEqual(other);
/*
Xanadu-Xpp-Basic.st:405:Heaper methodsFor: 'promise messages'!
{BooleanVar CLIENT login} equals: other {Heaper}
	"Return true if the two objects are equal."
	^self isEqual: other!
*/
}
public int hash() {
	return hashForEqual();
/*
Xanadu-Xpp-Basic.st:409:Heaper methodsFor: 'promise messages'!
{UInt32 CLIENT login} hash
	^self hashForEqual!
*/
}
/*
Xanadu-Xpp-Basic.st:412:Heaper methodsFor: 'promise messages'!
{BooleanVar CLIENT login} isKindOf.IntegerVar: catNum {IntegerVar}
	"The server message to test the run-time type of a promise."
	
	Dean shouldImplement!
*/
/*
Xanadu-Xpp-Basic.st:426:Heaper class methodsFor: 'automatic generation'!
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
*/
/*
Xanadu-Xpp-Basic.st:438:Heaper class methodsFor: 'automatic generation'!
{void} compileGeneratedClassMethod: text {String}
	self class compile: text
		classified: self generatedCategory
		notifying: nil.!
*/
/*
Xanadu-Xpp-Basic.st:443:Heaper class methodsFor: 'automatic generation'!
{void} compileGeneratedMethod: text {String}
	self compile: text
		classified: self generatedCategory
		notifying: nil.!
*/
/*
Xanadu-Xpp-Basic.st:448:Heaper class methodsFor: 'automatic generation'!
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
*/
/*
Xanadu-Xpp-Basic.st:465:Heaper class methodsFor: 'automatic generation'!
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
*/
/*
Xanadu-Xpp-Basic.st:487:Heaper class methodsFor: 'automatic generation'!
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
*/
/*
Xanadu-Xpp-Basic.st:525:Heaper class methodsFor: 'automatic generation'!
convertSubclassCopyDeclarations
	"This converts all copy classes to the new representation.  Rather than copy declaration for instance variables, the classes will have the COPY attribute, and instance variables that should not be copied have the nocopy attribute."
	
	"Heaper convertSubclassCopyDeclarations"
	
	self withAllSubclasses do: [:cl | cl convertCopyDeclarations]!
*/
/*
Xanadu-Xpp-Basic.st:532:Heaper class methodsFor: 'automatic generation'!
convertSubclassDeferredDeclarations
	"This converts all deferred classes to the new representation.  Rather than deferred declaration for protocols, the classes will have the DEFERRED attribute."
	
	"Heaper convertSubclassDeferredDeclarations"
	
	self withAllSubclasses do: [:cl | cl convertDeferredDeclarations]!
*/
/*
Xanadu-Xpp-Basic.st:539:Heaper class methodsFor: 'automatic generation'!
convertSubclassProxyDeclarations
	"This converts all deferred classes to the new representation.  Rather than proxy declaration for protocols, the classes will have the By.Proxy attribute."
	
	"Heaper convertSubclassProxyDeclarations"
	
	self withAllSubclasses do: [:cl | cl convertProxyDeclarations]!
*/
/*
Xanadu-Xpp-Basic.st:546:Heaper class methodsFor: 'automatic generation'!
generatedCategory
	^#'generated:'!
*/
/*
Xanadu-Xpp-Basic.st:549:Heaper class methodsFor: 'automatic generation'!
isGenerated
	"Automatically generated classes should implement this as true."
	
	^false!
*/
/*
Xanadu-Xpp-Basic.st:554:Heaper class methodsFor: 'automatic generation'!
removeGeneratedCode
	ChangeLog suppressLoggingIn:
		[self isGenerated
			ifTrue: [self removeFromSystem]
			ifFalse: [self removeCategory: #generated:.
					self class removeCategory: #generated:]]!
*/
/*
Xanadu-Xpp-Basic.st:561:Heaper class methodsFor: 'automatic generation'!
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
			RequestHandler class removeCategory: #'translate: generated']]!
*/
/*
Xanadu-Xpp-Basic.st:576:Heaper class methodsFor: 'stubble PSEUDO_COPY'!
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
*/
/*
Xanadu-Xpp-Basic.st:590:Heaper class methodsFor: 'stubble PSEUDO_COPY'!
make.Rcvr: rcvr
	self unimplemented!
*/
/*
Xanadu-Xpp-Basic.st:595:Heaper class methodsFor: 'stubble COPY'!
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
*/
/*
Xanadu-Xpp-Basic.st:613:Heaper class methodsFor: 'stubble COPY'!
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
*/
/*
Xanadu-Xpp-Basic.st:632:Heaper class methodsFor: 'stubble COPY'!
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
				initialSelection: nil]!
*/
/*
Xanadu-Xpp-Basic.st:650:Heaper class methodsFor: 'stubble'!
compileClientSubclasses
	"Heaper compileClientSubclasses"
	self cachePromiseNameTableIn: 
	[self withAllSubclasses do: [:class | 
		(class hasAttribute: #ON.CLIENT)
			ifTrue: [ChangeLog suppressLoggingIn: [class compilePromise]]]]!
*/
/*
Xanadu-Xpp-Basic.st:657:Heaper class methodsFor: 'stubble'!
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
*/
/*
Xanadu-Xpp-Basic.st:682:Heaper class methodsFor: 'stubble'!
compileSubclassStubbleMethods
	"compile subclass methods on me and all subclasses"
	"Heaper compileSubclassStubbleMethods"
	self cachePromiseNameTableIn:
	[self withAllSubclasses do: [ :class | 
		class compileStubbleMethods]]!
*/
/*
Xanadu-Xpp-Basic.st:689:Heaper class methodsFor: 'stubble'!
removeStubbleMethods
	ChangeLog suppressLoggingIn:
		[(self superclass name , 'Proxy' = self name)
			ifTrue: [self removeFromSystem]
			ifFalse: 
				[self removeCategory: #'smalltalk: stubble'.
				self removeCategory: #'smalltalk stubble'.]]!
*/
/*
Xanadu-Xpp-Basic.st:697:Heaper class methodsFor: 'stubble'!
removeSubclassStubbleMethods
	"remove subclass methods on me and all subclasses"
	"Heaper removeSubclassStubbleMethods"
	self withAllSubclasses do: [ :class |
		class removeStubbleMethods].
	self == Heaper ifTrue:
		[ChangeLog suppressLoggingIn:
			[RPCSpecialist removeCategory: #'translate: generated']]!
*/
/*
Xanadu-Xpp-Basic.st:708:Heaper class methodsFor: 'private: stubble accessing'!
{OrderedCollection of: Symbol} argumentTypesFor: selector {Selector}
	"the argument types for the given message"
	^(myArguments ~~ nil and: [myArguments includesKey: selector])
		ifTrue: [myArguments at: selector]
		ifFalse: [superclass argumentTypesFor: selector]!
*/
/*
Xanadu-Xpp-Basic.st:714:Heaper class methodsFor: 'private: stubble accessing'!
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
*/
/*
Xanadu-Xpp-Basic.st:730:Heaper class methodsFor: 'private: stubble accessing'!
{Boolean} definesProxyMethods
	^(self getCxxClassDescription includesAttribute: #BY.PROXY)
		and: [nil ~~ (self selectors
				detect: [:sel {Symbol} |
						self selector: sel hasAttribute: #PROXY]
				ifNone: [nil])]!
*/
/*
Xanadu-Xpp-Basic.st:737:Heaper class methodsFor: 'private: stubble accessing'!
{Boolean} hasProxyMethods
	^self definesProxyMethods
		or: [self ~~ Heaper and: [superclass hasProxyMethods]]!
*/
/*
Xanadu-Xpp-Basic.st:741:Heaper class methodsFor: 'private: stubble accessing'!
{Symbol} mangle: selector {Symbol}
	"return the C++ mangled function name from the Smalltalk selector"
	^(myTokens ~~ nil and: [myTokens includesKey: selector])
		ifTrue: [myTokens at: selector]
		ifFalse: [superclass mangle: selector]!
*/
/*
Xanadu-Xpp-Basic.st:747:Heaper class methodsFor: 'private: stubble accessing'!
{Symbol} returnTypeFor: selector {Selector}
	"the return type for the given message"
	self thingToDo. "either cache this or put a dictionary in a class instance variable"
	^(myReturn ~~ nil and: [myReturn includesKey: selector])
		ifTrue: [myReturn at: selector]
		ifFalse: [superclass returnTypeFor: selector]!
*/
/*
Xanadu-Xpp-Basic.st:754:Heaper class methodsFor: 'private: stubble accessing'!
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
*/
/*
Xanadu-Xpp-Basic.st:766:Heaper class methodsFor: 'private: stubble accessing'!
{Symbol} unmangle: token {Symbol}
	"return the Smalltalk selector from the C++ mangled function name"
	^(mySelectors ~~ nil and: [mySelectors includesKey: token])
		ifTrue: [mySelectors at: token]
		ifFalse: [superclass unmangle: token]!
*/
/*
Xanadu-Xpp-Basic.st:772:Heaper class methodsFor: 'private: stubble accessing'!
wipeStubble
	"remove stubble information"
	myTokens _ mySelectors _ myReturn _ myArguments _ nil.!
*/
/*
Xanadu-Xpp-Basic.st:891:Heaper class methodsFor: 'stubble REQUEST'!
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
*/
/*
Xanadu-Xpp-Basic.st:912:Heaper class methodsFor: 'stubble REQUEST'!
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
*/
/*
Xanadu-Xpp-Basic.st:963:Heaper class methodsFor: 'stubble REQUEST'!
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
*/
/*
Xanadu-Xpp-Basic.st:998:Heaper class methodsFor: 'stubble REQUEST'!
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
*/
/*
Xanadu-Xpp-Basic.st:1047:Heaper class methodsFor: 'stubble OTHERS'!
{void} compileEQ
	"Compile code to do EQ comparisions"
	self compileGeneratedMethod: 'isEqual: other ^self == other'.
	self compileGeneratedMethod: 'actualHashForEqual ^self asOop'!
*/
/*
Xanadu-Xpp-Basic.st:1052:Heaper class methodsFor: 'stubble OTHERS'!
{void} compileHook
	"compile changes to make this a hook class - add an instance variable and accessing methods"
	(self instVarNames includes: 'myPackagingCategory') ifFalse:
		[self addInstVarName: 'myPackagingCategory'.
		self getCxxClassDescription instVarTypeAt: 'myPackagingCategory' put: '{Category generated smalltalk}'].
	self compile: 'packagingCategory ^myPackagingCategory'
		classified: self generatedCategory notifying: nil.
	self compile: 'packagingCategory: cat myPackagingCategory := cat'
		classified: self generatedCategory notifying: nil.!
*/
/*
Xanadu-Xpp-Basic.st:1064:Heaper class methodsFor: 'stubble ON.CLIENT'!
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
*/
/*
Xanadu-Xpp-Basic.st:1081:Heaper class methodsFor: 'stubble ON.CLIENT'!
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
*/
/*
Xanadu-Xpp-Basic.st:1138:Heaper class methodsFor: 'stubble ON.CLIENT'!
compilePromise
	"make a promise class and generate all its methods."
	
	self == Heaper ifTrue: [^VOID].  "hack so we can have a Heaper ON.CLIENT declaration."
	self cachePromiseNameTableIn: 
	[| promise {Class} |
	promise _ self compilePromiseClass.
	self compileConstantPromiseMethods: promise.
	self compilePromiseMethods: promise.
	self compilePromiseFluidDeclarations: promise]!
*/
/*
Xanadu-Xpp-Basic.st:1174:Heaper class methodsFor: 'stubble ON.CLIENT'!
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
*/
/*
Xanadu-Xpp-Basic.st:1217:Heaper class methodsFor: 'stubble ON.CLIENT'!
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
*/
/*
Xanadu-Xpp-Basic.st:1247:Heaper class methodsFor: 'stubble ON.CLIENT'!
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
*/
/*
Xanadu-Xpp-Basic.st:1338:Heaper class methodsFor: 'stubble ON.CLIENT'!
isIntType: type {Symbol}
	^ #(IntegerVar Int32 UInt32 Int8 UInt8) includes: type!
*/
/*
Xanadu-Xpp-Basic.st:1341:Heaper class methodsFor: 'stubble ON.CLIENT'!
{BooleanVar} isRawType: type {TypeDescription}
	^type refType = #star and: [type baseType = 'char' or: [type baseType = 'void']]!
*/
/*
Xanadu-Xpp-Basic.st:1345:Heaper class methodsFor: 'stubble ON.CLIENT'!
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
		ifFalse: [^ defaultValue].!
*/
/*
Xanadu-Xpp-Basic.st:1363:Heaper class methodsFor: 'promise name table'!
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
*/
/*
Xanadu-Xpp-Basic.st:1404:Heaper class methodsFor: 'promise name table'!
cachePromiseNameTableIn: aBlock
	"evaluate a block with the cache enabled"
	PromiseNameTable notNil
		ifTrue: [aBlock value]
		ifFalse:
			[self cachePromiseNameTable.
			aBlock valueNowOrOnUnwindDo:
				[self flushPromiseNameTable]]!
*/
/*
Xanadu-Xpp-Basic.st:1413:Heaper class methodsFor: 'promise name table'!
{String} exportName
	"Return the completely stripped rootName for this class.  Get rid of prefixes. 
	 Some classes will implement this directly in order to provide another name."
	(self == Heaper or: [self hasAttribute: #ON.CLIENT]) ifTrue: [^self rootName].
	^self superclass exportName!
*/
/*
Xanadu-Xpp-Basic.st:1420:Heaper class methodsFor: 'promise name table'!
{IdentityDictionary} flushPromiseNameTable
	"flush the promise name table."
	"Heaper flushPromiseNameTable"
	PromiseNameTable _ nil!
*/
/*
Xanadu-Xpp-Basic.st:1426:Heaper class methodsFor: 'promise name table'!
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
*/
/*
Xanadu-Xpp-Basic.st:1444:Heaper class methodsFor: 'promise name table'!
{Symbol} parseExportName: pNode {ProgramNode}  
	| type {TypeDescription} sym {Symbol} |
	type _ TypeDescription fromParse: pNode.
	sym _ type baseType asSymbol.
	(sym = #char and: [type refType = #star]) ifTrue: [^#'char star']
	ifFalse: [(sym = #void and: [type refType = #star]) ifTrue: [^#'void star']
	ifFalse: [sym = #void ifTrue: [^'Void']
	ifFalse: [sym = #NOACK ifTrue: [^'NOACK']
	ifFalse: [^(Smalltalk at: sym ifAbsent: [self warn: 'Symbol not client or promise: ', sym, ' in: ' , self name. Heaper]) exportName]]]]!
*/
/*
Xanadu-Xpp-Basic.st:1454:Heaper class methodsFor: 'promise name table'!
{Class} promiseClass
	"starting at self and moving up the hierarchy, find the first ON.CLIENT class, and return its promise counterpart.  
	BLAST if it should exist and doesn't.  "
	"?? returns nil if it should but there's something else in the global dictionary by that name.???  "
	|  promiseTable |
	promiseTable _ self promiseNameTable.
	^Smalltalk at: (promiseTable at: self name ifAbsent: [#XuPromise])!
*/
/*
Xanadu-Xpp-Basic.st:1463:Heaper class methodsFor: 'promise name table'!
{Symbol} promiseName
	"Return the name of the promise class associated generated for 'name'."
	^('Xu' , self exportName) asSymbol!
*/
/*
Xanadu-Xpp-Basic.st:1468:Heaper class methodsFor: 'promise name table'!
{IdentityDictionary} promiseNameTable
	Heaper cachePromiseNameTableIn: [^PromiseNameTable]!
*/
/*
Xanadu-Xpp-Basic.st:1471:Heaper class methodsFor: 'promise name table'!
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
*/
/*
Xanadu-Xpp-Basic.st:1489:Heaper class methodsFor: 'promise name table'!
{String} serverNameFor: root {String}
	"Return the name of the server side class that implements 'root'"
	root = #NOACK ifTrue: [^'NOACK'].
	(root testSuffix: 'OrNull')
		ifTrue: [^self promiseNameTable at: ('Xu' , (root copyFrom: 1 to: (root size - 6))) asSymbol]
		ifFalse: [^self promiseNameTable at: ('Xu' , root) asSymbol]!
*/
/*
Xanadu-Xpp-Basic.st:1497:Heaper class methodsFor: 'promise name table'!
{Symbol} togglePromiseName: sym {Symbol}  
	Heaper cachePromiseNameTableIn:
		[^PromiseNameTable at: sym ifAbsent: [self warn: 'Symbol not client or promise: ', sym, ' in: ' , self name.  #Heaper]]!
*/
/*
Xanadu-Xpp-Basic.st:1501:Heaper class methodsFor: 'promise name table'!
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
						ifFalse: [^ prThing ]]]]!
*/
/*
Xanadu-Xpp-Basic.st:1527:Heaper class methodsFor: 'client generation'!
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
*/
/*
Xanadu-Xpp-Basic.st:1584:Heaper class methodsFor: 'client generation'!
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
*/
/*
Xanadu-Xpp-Basic.st:1597:Heaper class methodsFor: 'client generation'!
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
*/
/*
Xanadu-Xpp-Basic.st:1618:Heaper class methodsFor: 'client generation'!
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
*/
/*
Xanadu-Xpp-Basic.st:1634:Heaper class methodsFor: 'client generation'!
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
*/
/*
Xanadu-Xpp-Basic.st:1793:Heaper class methodsFor: 'client generation'!
promiseToAbstract: token
	"token = #IntValue 		ifTrue: [^#IntegerVar]."
	token = #BooleanValue	ifTrue: [^#BooleanVar].
	token = #Void 			ifTrue: [^#Void].
	(token testSuffix: 'OrNull') ifTrue: [^#HeaperOrNull].
	^#Heaper!
*/
/*
Xanadu-Xpp-Basic.st:1800:Heaper class methodsFor: 'client generation'!
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
*/
/*
Xanadu-Xpp-Basic.st:1860:Heaper class methodsFor: 'client generation'!
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
*/
/*
Xanadu-Xpp-Basic.st:1878:Heaper class methodsFor: 'client generation'!
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
			ifFalse: [argCount-2 timesRepeat: [oo << 'with:']]]) asSymbol!
*/
/*
Xanadu-Xpp-Basic.st:1893:Heaper class methodsFor: 'client freeze'!
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
*/
/*
Xanadu-Xpp-Basic.st:1910:Heaper class methodsFor: 'client freeze'!
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
*/
/*
Xanadu-Xpp-Basic.st:1922:Heaper class methodsFor: 'client freeze'!
clientProtocol
	"IntegerSpace clientProtocol"
	"Compute the client protocol."
	
	^String streamContents: [:oo | self clientProtocolOn: oo]!
*/
/*
Xanadu-Xpp-Basic.st:1928:Heaper class methodsFor: 'client freeze'!
fileOutClientProtocol: filename
	| file |
	PromiseNameTable _ nil.
	file _ (Filename named: filename) writeStream.
	[Heaper allClientProtocolOn: file]
		valueNowOrOnUnwindDo: [file close]!
*/
/*
Xanadu-Xpp-Basic.st:1935:Heaper class methodsFor: 'client freeze'!
freezeClientClasses
	"Heaper freezeClientClasses."
	
	(self confirm: 'Do you really want to do this?') ifFalse: [^VOID].
	(self confirm: 'Are you sure?') ifFalse: [^VOID].
	Heaper cachePromiseNameTableIn: 
	[Heaper at: #clientClasses storeInfo: 
		(String streamContents: [:oo |
			self clientClassesDo: [:assoc | oo << assoc key << ' ']])]!
*/
/*
Xanadu-Xpp-Basic.st:1945:Heaper class methodsFor: 'client freeze'!
freezeClientProtocol
	"Heaper freezeClientProtocol."
	
	(self confirm: 'Do you really want to do this?') ifFalse: [^VOID].
	(self confirm: 'Are you sure?') ifFalse: [^VOID].
	self withAllSubclasses do: [:class | 
		(class hasAttribute: #ON.CLIENT) ifTrue:
			[class at: #frozenProtocol storeInfo: class clientProtocol]]!
*/
/*
Xanadu-Xpp-Basic.st:1954:Heaper class methodsFor: 'client freeze'!
freezeStProtocol
	"Heaper freezeStProtocol."
	
	(self confirm: 'Do you really want to do this?') ifFalse: [^VOID].
	(self confirm: 'Are you sure?') ifFalse: [^VOID].
	self withAllSubclasses do: [:cl | 
		(cl hasAttribute: #ON.CLIENT) ifTrue:
			[cl at: #stProtocol storeInfo: cl stClientProtocol]]!
*/
/*
Xanadu-Xpp-Basic.st:1963:Heaper class methodsFor: 'client freeze'!
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
*/
/*
Xanadu-Xpp-Basic.st:1996:Heaper class methodsFor: 'client freeze'!
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
	passed ifTrue: [Transcript cr; show: 'Protocol verification passed.']!
*/
/*
Xanadu-Xpp-Basic.st:2013:Heaper class methodsFor: 'client freeze support'!
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
*/
/*
Xanadu-Xpp-Basic.st:2041:Heaper class methodsFor: 'client freeze support'!
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
*/
/*
Xanadu-Xpp-Basic.st:2067:Heaper class methodsFor: 'client freeze support'!
clientProtocolOn: oo
	"String streamContents: [:oo | FeStatusDetector clientProtocolOn: oo]"
	"Compute the client protocol."
	
	self clientProtocolOn: oo with: 0!
*/
/*
Xanadu-Xpp-Basic.st:2073:Heaper class methodsFor: 'client freeze support'!
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
*/
/*
Xanadu-Xpp-Basic.st:2089:Heaper class methodsFor: 'client freeze support'!
frozenClasses
	"Heaper frozenClasses."
	
	| clients stream  |
	clients _ OrderedCollection new.
	stream _ (Heaper getInfo: #promiseClasses) readStream.
	[stream atEnd] whileFalse:
		[clients addLast: (stream upTo: Character space)].
	^clients!
*/
/*
Xanadu-Xpp-Basic.st:2320:Heaper class methodsFor: 'smalltalk: initialization'!
cleanupGarbage
	"Heaper cleanupGarbage"
	GarbageCount _ IntegerVar0.
	InitializedClasses _ NULL.
	AllBlasts := nil.
	PackageTable _ NULL.
	LastMemory _ NULL!
*/
/*
Xanadu-Xpp-Basic.st:2328:Heaper class methodsFor: 'smalltalk: initialization'!
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
*/
/*
Xanadu-Xpp-Basic.st:2346:Heaper class methodsFor: 'smalltalk: initialization'!
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
*/
/*
Xanadu-Xpp-Basic.st:2358:Heaper class methodsFor: 'smalltalk: initialization'!
initializedClasses
	^InitializedClasses!
*/
/*
Xanadu-Xpp-Basic.st:2361:Heaper class methodsFor: 'smalltalk: initialization'!
initializingClasses
	^InitializingClasses!
*/
/*
Xanadu-Xpp-Basic.st:2364:Heaper class methodsFor: 'smalltalk: initialization'!
initPackages
	myPackageClasses _ nil!
*/
/*
Xanadu-Xpp-Basic.st:2368:Heaper class methodsFor: 'smalltalk: initialization'!
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
*/
/*
Xanadu-Xpp-Basic.st:2380:Heaper class methodsFor: 'smalltalk: initialization'!
linkTimeNonInherited
	MACINTOSH _ XVIEW _ false.
	GarbageCount _ IntegerVar0.
	InGC _ false.
	BecomeMap _ IdentityDictionary new.
	MAX.U.CATEGORY.U.NAME _ 4088.
	PackageTable _ IdentityDictionary new.
	LastMemory _ NULL.
	self computePreorder: 0.!
*/
/*
Xanadu-Xpp-Basic.st:2392:Heaper class methodsFor: 'global: accessing'!
{void} canYouBecome: cat {Category} 
	| set {MuSet} |
	((self isEqual: cat) or: [(self isEqual: SuspendedHeaper) or: [cat isEqual: SuspendedHeaper]])
		ifTrue: [^true].
	set _ BecomeMap at: self ifAbsent: [NULL].
	^(set ~~ NULL and: [set includes: cat]) or: [self ~~ Heaper and: [self superclass canYouBecome: cat]]!
*/
/*
Xanadu-Xpp-Basic.st:2400:Heaper class methodsFor: 'global: accessing'!
{BooleanVar} compare: a {Heaper | NULL} with: b {Heaper | NULL}
	^a == b or: [a ~~ NULL and: [b ~~ NULL and: [a isEqual: b]]]!
*/
/*
Xanadu-Xpp-Basic.st:2403:Heaper class methodsFor: 'global: accessing'!
fetchSuperCategory
	self == Heaper ifTrue: [^NULL] ifFalse: [^self superclass]!
*/
/*
Xanadu-Xpp-Basic.st:2406:Heaper class methodsFor: 'global: accessing'!
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
*/
/*
Xanadu-Xpp-Basic.st:2420:Heaper class methodsFor: 'global: accessing'!
getSuperCategory
	(self == Heaper) not assert.
	^superclass!
*/
/*
Xanadu-Xpp-Basic.st:2424:Heaper class methodsFor: 'global: accessing'!
{Uint3} instanceSize
	^self instSize * 4!
*/
/*
Xanadu-Xpp-Basic.st:2427:Heaper class methodsFor: 'global: accessing'!
{Boolean} isEqualOrSubclassOf: cat {Category}
	^ self == cat or: [self inheritsFrom: cat]!
*/
/*
Xanadu-Xpp-Basic.st:2431:Heaper class methodsFor: 'global: accessing'!
{void} mayBecome: cat {Category} 
	| set {MuSet of: Category} |
	set _ BecomeMap at: self ifAbsent: 
		[set _ Set new.
		BecomeMap at: self put: set].
	set add: cat!
*/
/*
Xanadu-Xpp-Basic.st:2438:Heaper class methodsFor: 'global: accessing'!
{void} mayBecomeAnySubclassOf: cat {Category} 
	| set {MuSet} |
	set _ BecomeMap at: self ifAbsent: 
		[set _ Set new.
		BecomeMap at: self put: set].
	cat withAllSubclasses do: [:sub | set add: sub]!
*/
/*
Xanadu-Xpp-Basic.st:2447:Heaper class methodsFor: 'smalltalk: creation'!
create
	"Create the frame and call the constructor defined for instances."
	^self new create!
*/
/*
Xanadu-Xpp-Basic.st:2451:Heaper class methodsFor: 'smalltalk: creation'!
create.IntegerVar: arg1
	"Create the frame and call the constructor defined for instances."
	^self new create.IntegerVar: arg1!
*/
/*
Xanadu-Xpp-Basic.st:2456:Heaper class methodsFor: 'smalltalk: creation'!
{void} create.Rcvr: rcvr {Rcvr}
	^self new create.Rcvr: rcvr!
*/
/*
Xanadu-Xpp-Basic.st:2459:Heaper class methodsFor: 'smalltalk: creation'!
create: arg1
	"Create the frame and call the constructor defined for instances."
	^self new create: arg1!
*/
/*
Xanadu-Xpp-Basic.st:2464:Heaper class methodsFor: 'smalltalk: creation'!
create: arg1 with: arg2
	"Create the frame and call the constructor defined for instances."
	^self new create: arg1 with: arg2!
*/
/*
Xanadu-Xpp-Basic.st:2469:Heaper class methodsFor: 'smalltalk: creation'!
create: arg1 with: arg2 with: arg3
	"Create the frame and call the constructor defined for instances."
	^self new create: arg1 with: arg2 with: arg3!
*/
/*
Xanadu-Xpp-Basic.st:2474:Heaper class methodsFor: 'smalltalk: creation'!
create: arg1 with: arg2 with: arg3 with: arg4
	"Create the frame and call the constructor defined for instances."
	^self new create: arg1 with: arg2 with: arg3 with: arg4!
*/
/*
Xanadu-Xpp-Basic.st:2479:Heaper class methodsFor: 'smalltalk: creation'!
create: arg1 with: arg2 with: arg3 with: arg4 with: arg5
	"Create the frame and call the constructor defined for instances."
	^self new create: arg1 with: arg2 with: arg3 with: arg4 with: arg5!
*/
/*
Xanadu-Xpp-Basic.st:2484:Heaper class methodsFor: 'smalltalk: creation'!
create: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6
	"Create the frame and call the constructor defined for instances."
	^self new create: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6!
*/
/*
Xanadu-Xpp-Basic.st:2489:Heaper class methodsFor: 'smalltalk: creation'!
create: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6 with: arg7
	"Create the frame and call the constructor defined for instances."
	^self new create: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6 with: arg7!
*/
/*
Xanadu-Xpp-Basic.st:2494:Heaper class methodsFor: 'smalltalk: creation'!
create: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6 with: arg7 with: arg8
	"Create the frame and call the constructor defined for instances."
	^self new create: arg1 with: arg2 with: arg3 with: arg4 with: arg5 with: arg6 with: arg7 with: arg8!
*/
/*
Xanadu-Xpp-Basic.st:2499:Heaper class methodsFor: 'smalltalk: creation'!
new.AllocType: allocType {Symbol} 
	"This does nothing in smalltalk, but allows translated code to specify 
	how a new object is allocated. Returns self since Heaper creates do 
	^self new create..."
	^self!
*/
/*
Xanadu-Xpp-Basic.st:2506:Heaper class methodsFor: 'smalltalk: creation'!
{Heaper} new.Become: memory {Heaper | DeletedHeaper} 
	"Create the frame for a new instance using the identity of memory. This 
	corresponds to creating the frame in the actual memory in c++. First call the 
	destructor for memory."
	memory class ~~ DeletedHeaper ifTrue:   "This takes care of the void star case."
		[(memory getCategory canYouBecome: self) assert: 'Can only become something that was explicitly allowed.'.
		memory destructor].
	(LastMemory _ self new) become: memory.
	^memory!
*/
/*
Xanadu-Xpp-Basic.st:2519:Heaper class methodsFor: 'global: blasts'!
BLAST: problem {Symbol}
	(self signal: problem) raise!
*/
/*
Xanadu-Xpp-Basic.st:2522:Heaper class methodsFor: 'global: blasts'!
blast: problem {Problem}
	"raise an exception"
	
	(Heaper signal: problem getProblemName asSymbol) raiseWith: problem!
*/
/*
Xanadu-Xpp-Basic.st:2527:Heaper class methodsFor: 'global: blasts'!
PROBLEM: exception
	^exception signal!
*/
/*
Xanadu-Xpp-Basic.st:2531:Heaper class methodsFor: 'global: blasts'!
problems.AllBlasts
	"A ProblemList for all X++ BLASTs"
	
	XppBlast == nil ifTrue:
			[XppBlast := Signal new nameClass: 'BLAST' message: #All; notifierString: 'BLAST(!!!!)'].
	^XppBlast!
*/
/*
Xanadu-Xpp-Basic.st:2538:Heaper class methodsFor: 'global: blasts'!
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
*/
/*
Xanadu-Xpp-Basic.st:2550:Heaper class methodsFor: 'global: blasts'!
{Signal} signals: signals {Array of: Symbol}
	"Define a set of blasts blast and return a SignalCollection."
	
	| result |
	signals size = 1 ifTrue:
		[^self signal: signals first].
	result := SignalCollection new.
	signals do: [ :each |
		result add: (self signal: each)].
	^result!
*/
/*
Xanadu-Xpp-Basic.st:2563:Heaper class methodsFor: 'hooks: global: xpp hooks'!
IntegerVar: x
	^x!
*/
/*
Xanadu-Xpp-Basic.st:2566:Heaper class methodsFor: 'hooks: global: xpp hooks'!
{void} scheduleTermination
	"In xpp, smainx.cxx defines this to set a flag requesting server termination after current request."
	self thingToDo.
	"This should do something in smalltalk?"!
*/
/*
Xanadu-Xpp-Basic.st:2573:Heaper class methodsFor: 'global: garbage collection'!
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
*/
/*
Xanadu-Xpp-Basic.st:2601:Heaper class methodsFor: 'global: garbage collection'!
garbageCollect
	| roots |
	roots _ ActualHand allInstances.
	roots add: CurrentGrandMap fluidFetch.
	roots addAll: PhantomHand allInstances.
	self garbageCollectFrom: roots!
*/
/*
Xanadu-Xpp-Basic.st:2608:Heaper class methodsFor: 'global: garbage collection'!
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
*/
/*
Xanadu-Xpp-Basic.st:2656:Heaper class methodsFor: 'global: garbage collection'!
{void} gcOpportunity
	"A no-op method in smalltalk to tell c++ it is a good time to garbage collect."!
*/
/*
Xanadu-Xpp-Basic.st:2659:Heaper class methodsFor: 'global: garbage collection'!
{void} gcOpportunity: anInt
	"A no-op method in smalltalk to tell c++ it is a good time to garbage collect."
	"The arguments for now are -1 means do it now, n>0 means on the n-th alloc,
	otherwise it's up to the receiver."!
*/
/*
Xanadu-Xpp-Basic.st:2664:Heaper class methodsFor: 'global: garbage collection'!
{BooleanVar} inGC
	"Some destructors need to have different behavior during garbage 
	collection, since some of their instance variables may already have 
	been collected. In X++, this will return true during garbage 
	collection."
	^InGC!
*/
/*
Xanadu-Xpp-Basic.st:2672:Heaper class methodsFor: 'global: garbage collection'!
{BooleanVar} isConstructed: obj {void star}
	^obj ~~ NULL and: [obj class ~~ DeletedHeaper]!
*/
/*
Xanadu-Xpp-Basic.st:2675:Heaper class methodsFor: 'global: garbage collection'!
{BooleanVar} isDestructed: obj {void star}
	^obj == NULL or: [obj class == DeletedHeaper]!
*/
/*
Xanadu-Xpp-Basic.st:2678:Heaper class methodsFor: 'global: garbage collection'!
{void} setGC: flag {BooleanVar}
	"Some destructors need to have different behavior during garbage 
	collection, since some of their instance variables may already have 
	been collected. In X++, this will return true during garbage 
	collection."
	^InGC _ flag!
*/
/*
Xanadu-Xpp-Basic.st:2688:Heaper class methodsFor: 'global: packages'!
{void} addPackageCategory: packageCat {PackageCategory}
	"add a package category to my list"
	self packageClasses add: packageCat!
*/
/*
Xanadu-Xpp-Basic.st:2692:Heaper class methodsFor: 'global: packages'!
{Symbol} fetchAttribute: attr
	"get information attached to the given attribute, or NULL if none"
	^self getCxxClassDescription fetchAttribute: attr!
*/
/*
Xanadu-Xpp-Basic.st:2696:Heaper class methodsFor: 'global: packages'!
inspectPieces
	^self subclasses asOrderedCollection , self packageClasses!
*/
/*
Xanadu-Xpp-Basic.st:2699:Heaper class methodsFor: 'global: packages'!
{OrderedCollection of: Category} packageClasses
	"return my packages classes and create if needed"
	(myPackageClasses == nil) ifTrue:
		[myPackageClasses _ OrderedCollection new].
	^myPackageClasses!
*/
/*
Xanadu-Xpp-Basic.st:2705:Heaper class methodsFor: 'global: packages'!
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
			^self]!
*/
/*
Xanadu-Xpp-Basic.st:2720:Heaper class methodsFor: 'stubble tools'!
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
*/
/*
Xanadu-Xpp-Basic.st:2742:Heaper class methodsFor: 'stubble tools'!
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
*/
/*
Xanadu-Xpp-Basic.st:2763:Heaper class methodsFor: 'stubble tools'!
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
*/
/*
Xanadu-Xpp-Basic.st:2782:Heaper class methodsFor: 'stubble tools'!
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
*/
/*
Xanadu-Xpp-Basic.st:2795:Heaper class methodsFor: 'stubble tools'!
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
	^nocopy!
*/
/*
Xanadu-Xpp-Basic.st:2810:Heaper class methodsFor: 'smalltalk: passe'!
constant: constant {Symbol} type: type {String} value: value {Heaper}
	"define a global constant"
	"Ignore for the moment because otherwise initialization will fail."!
*/
/*
Xanadu-Xpp-Basic.st:2814:Heaper class methodsFor: 'smalltalk: passe'!
makeFillTable: filename
	self passe. "use makeRequestTable:"!
*/
/*
Xanadu-Xpp-Basic.st:2819:Heaper class methodsFor: 'global: preorder'!
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
*/
/*
Xanadu-Xpp-Basic.st:2831:Heaper class methodsFor: 'global: preorder'!
preorderMax
	^myPreorderMax!
*/
/*
Xanadu-Xpp-Basic.st:2834:Heaper class methodsFor: 'global: preorder'!
preorderNumber
	^myPreorderNumber!
*/
/*
Xanadu-Xpp-Basic.st:2839:Heaper class methodsFor: 'function pointers'!
pointerToStaticMember: selector {Symbol}
	^StaticFunctionPointer make: self with: selector!
*/
/*
Xanadu-Xpp-Basic.st:2843:Heaper class methodsFor: 'function pointers'!
pointerToStaticMember: selector {Symbol} with: typeString {String}
	^self pointerToStaticMember: selector!
*/
/*
Xanadu-Xpp-Basic.st:2847:Heaper class methodsFor: 'function pointers'!
pointerToVirtualMember: selector {Symbol}
	^selector!
*/
/*
Xanadu-Xpp-Basic.st:2853:Heaper class methodsFor: 'smalltalk: system'!
info.clientClasses
"XuHeaper XuAdminer XuArchiver XuBundle XuArrayBundle XuElementBundle XuPlaceHolderBundle XuCalc XuCalcCreator XuCoordinateSpace XuCrossSpace XuFilterSpace XuIDSpace XuIntegerSpace XuRealSpace XuSequenceSpace XuFillRangeDetector XuFillDetector XuGateKeeper XuServer XuKeyMaster XuLock XuBooLock XuChallengeLock XuMatchLock XuMultiLock XuWallLock XuMapping XuCrossMapping XuIntegerMapping XuSequenceMapping XuOrderSpec XuCrossOrderSpec XuPopSensor XuPosition XuFilterPosition XuID XuInteger XuReal XuSequence XuTuple XuPrimArray XuPrimSpec XuPrimFloatSpec XuPrimIntegerSpec XuPrimPointerSpec XuPrimValue XuPrimFloat XuPrimInteger XuRangeElement XuDataHolder XuEdition XuIDHolder XuLabel XuWork XuClub XuRational XuRegion XuCrossRegion XuFilter XuIDRegion XuIntegerRegion XuRealRegion XuSequenceRegion XuRevisionDetector XuSession XuStatusDetector XuStepper XuTableStepper XuWaitDetector XuWrapper XuClubDescription XuHyperLink XuHyperRef XuMultiRef XuSingleRef XuLockSmith XuBooLockSmith XuChallengeLockSmith XuMatchLockSmith XuMultiLockSmith XuWallLockSmith XuPath XuSet XuText XuWrapperSpec "!
*/
/*
Xanadu-Xpp-Basic.st:2856:Heaper class methodsFor: 'smalltalk: system'!
info.clientSideClasses
"FillRangeDetector FillDetector RevisionDetector StatusDetector WaitDetector Wrapper"!
*/
/*
Xanadu-Xpp-Basic.st:2859:Heaper class methodsFor: 'smalltalk: system'!
info.promiseClasses
"Promise Adminer Archiver Array FloatArray HumberArray IntArray PtrArray Bundle ArrayBundle ElementBundle PlaceHolderBundle CoordinateSpace CrossSpace FilterSpace IDSpace IntegerSpace RealSpace SequenceSpace FillRangeDetector FillDetector KeyMaster Lock BooLock ChallengeLock MatchLock MultiLock WallLock Mapping CrossMapping IntegerMapping SequenceMapping OrderSpec CrossOrderSpec Position FilterPosition ID Sequence Tuple Integer Real RangeElement DataHolder Edition IDHolder Label Work Club RevisionDetector Server Session StatusDetector Stepper TableStepper WaitDetector Wrapper ClubDescription HyperLink HyperRef MultiRef SingleRef LockSmith BooLockSmith ChallengeLockSmith MatchLockSmith MultiLockSmith WallLockSmith Path Set Text WrapperSpec Region CrossRegion Filter IDRegion IntegerRegion RealRegion SequenceRegion Value FloatValue IntValue"!
*/
/*
Xanadu-Xpp-Basic.st:2862:Heaper class methodsFor: 'smalltalk: system'!
info.stProtocol
"{void NOLOCK CLIENT} destroy
{UInt32 CLIENT} actualHashForEqual
{BooleanVar CLIENT} isEqual: other {Heaper}
"!
*/
/*
Xanadu-Xpp-Basic.st:2870:Heaper class methodsFor: 'accessing'!
{UInt32} takeOop
	^ self asOop!
*/
public static boolean isDestructed(Object obj) {
	return obj == null || obj instanceof DeletedHeaper;
/*

Generated during transformation: AddMethod
*/
}
public static boolean isConstructed(Object obj) {
	return obj != null && ! (obj instanceof DeletedHeaper);
/*

Generated during transformation: AddMethod
*/
}
public boolean equals(Object o) {
	if (o instanceof Heaper) {
		return equals((Heaper) o);
	}
	else {
		return false;
	}
/*

Generated during transformation: AddMethod
*/
}
public static String[] classHierarchy() {
	return new String[] 
	{"info.dgjones.abora.gold.xpp.basic.Heaper", "info.dgjones.abora.gold.primtab.PrimIndexTable", "info.dgjones.abora.gold.collection.settable.TableEntry", "info.dgjones.abora.gold.collection.settable.IndexEntry", "info.dgjones.abora.gold.collection.settable.PositionEntry", "info.dgjones.abora.gold.collection.settable.HashIndexEntry", "info.dgjones.abora.gold.collection.settable.HeaperAsEntry", "info.dgjones.abora.gold.traces.TracePosition", "info.dgjones.abora.gold.traces.BoundedTrace", "info.dgjones.abora.gold.collection.tables.ScruTable", "info.dgjones.abora.gold.collection.tables.MuTable", "info.dgjones.abora.gold.collection.tables.IntegerTable", "info.dgjones.abora.gold.collection.tables.OberIntegerTable", "info.dgjones.abora.gold.collection.tables.ActualIntegerTable", "info.dgjones.abora.gold.collection.tables.COWIntegerTable", "info.dgjones.abora.gold.collection.tables.MuArray", "info.dgjones.abora.gold.collection.tables.ActualArray", "info.dgjones.abora.gold.collection.grand.GrandHashTable", "info.dgjones.abora.gold.collection.tables.HashTable", "info.dgjones.abora.gold.collection.tables.ActualHashTable", "info.dgjones.abora.gold.collection.tables.OffsetScruArray", "info.dgjones.abora.gold.collection.tables.OffsetScruTable", "info.dgjones.abora.gold.collection.tables.ImmuTable", "info.dgjones.abora.gold.collection.tables.ImmuTableOnMu", "info.dgjones.abora.gold.collection.tables.OffsetImmuTable", "info.dgjones.abora.gold.collection.tables.IntegerScruTable", "info.dgjones.abora.gold.wrapper.FeWrapperSpec", "info.dgjones.abora.gold.wrapper.FeAbstractWrapperSpec", "info.dgjones.abora.gold.wrapper.FeConcreteWrapperSpec", "info.dgjones.abora.gold.wrapper.FeDirectWrapperSpec", "info.dgjones.abora.gold.wrapper.FeIndirectWrapperSpec", "info.dgjones.abora.gold.snarf.FlockLocation", "info.dgjones.abora.gold.snarf.FlockInfo", "info.dgjones.abora.gold.snarf.TestFlockInfo", "info.dgjones.abora.gold.props.PropChange", "info.dgjones.abora.gold.props.EndorsementsChange", "info.dgjones.abora.gold.props.CannotPartializeChange", "info.dgjones.abora.gold.props.DetectorWaitingChange", "info.dgjones.abora.gold.props.FullPropChange", "info.dgjones.abora.gold.props.SensorPropChange", "info.dgjones.abora.gold.props.BertPropChange", "info.dgjones.abora.gold.props.PermissionsChange", "info.dgjones.abora.gold.cache.SuspendedHeaper", "info.dgjones.abora.gold.proman.ExceptionRecord", "info.dgjones.abora.gold.negoti8.ProtocolBroker", "info.dgjones.abora.gold.be.canopy.prop.Prop", "info.dgjones.abora.gold.be.canopy.prop.BertProp", "info.dgjones.abora.gold.be.canopy.prop.SensorProp", "info.dgjones.abora.gold.urdi.SnarfInfoHandler", "info.dgjones.abora.gold.filter.Joint", "info.dgjones.abora.gold.proman.ByteShuffler", "info.dgjones.abora.gold.proman.SimpleShuffler", "info.dgjones.abora.gold.proman.NoShuffler", "info.dgjones.abora.gold.be.locks.Lock", "info.dgjones.abora.gold.be.locks.MatchLock", "info.dgjones.abora.gold.be.locks.BooLock", "info.dgjones.abora.gold.be.locks.ChallengeLock", "info.dgjones.abora.gold.be.locks.WallLock", "info.dgjones.abora.gold.be.locks.MultiLock", "info.dgjones.abora.gold.xpp.packages.Package", "info.dgjones.abora.gold.detect.FeDetector", "info.dgjones.abora.gold.detect.FeFillRangeDetector", "info.dgjones.abora.gold.nkernel.WorksTestFillRangeDetector", "info.dgjones.abora.gold.proman.CommFillRangeDetector", "info.dgjones.abora.gold.detect.FeFillDetector", "info.dgjones.abora.gold.nkernel.WorksTestFillDetector", "info.dgjones.abora.gold.proman.CommFillDetector", "info.dgjones.abora.gold.detect.FeWaitDetector", "info.dgjones.abora.gold.nkernel.WorksWaitDetector", "info.dgjones.abora.gold.proman.CommWaitDetector", "info.dgjones.abora.gold.detect.FeStatusDetector", "info.dgjones.abora.gold.proman.CommStatusDetector", "info.dgjones.abora.gold.nkernel.WorksTestStatusDetector", "info.dgjones.abora.gold.detect.FeRevisionDetector", "info.dgjones.abora.gold.proman.CommRevisionDetector", "info.dgjones.abora.gold.spaces.basic.Position", "info.dgjones.abora.gold.spaces.basic.UnOrdered", "info.dgjones.abora.gold.spaces.unordered.HeaperAsPosition", "info.dgjones.abora.gold.spaces.unordered.StrongAsPosition", "info.dgjones.abora.gold.tumbler.Sequence", "info.dgjones.abora.gold.tumbler.RealPos", "info.dgjones.abora.gold.tumbler.IEEE32Pos", "info.dgjones.abora.gold.tumbler.IEEE64Pos", "info.dgjones.abora.gold.tumbler.IEEE8Pos", "info.dgjones.abora.gold.be.basic.ID", "info.dgjones.abora.gold.filter.FilterPosition", "info.dgjones.abora.gold.spaces.integers.IntegerPos", "info.dgjones.abora.gold.spaces.cross.Tuple", "info.dgjones.abora.gold.spaces.cross.ActualTuple", "info.dgjones.abora.gold.collection.cache.HashSetCache", "info.dgjones.abora.gold.xcvr.CommIbid", "info.dgjones.abora.gold.rcmain.ServerChunk", "info.dgjones.abora.gold.proman.ExecutePromiseFile", "info.dgjones.abora.gold.rcmain.FDListener", "info.dgjones.abora.gold.rcmain.IPRendezvousListener", "info.dgjones.abora.gold.rcmain.IPPromiseListener", "info.dgjones.abora.gold.srvloop.TestChunk", "info.dgjones.abora.gold.xcvr.XcvrMaker", "info.dgjones.abora.gold.xcvr.TextyXcvrMaker", "info.dgjones.abora.gold.xcvr.BogusXcvrMaker", "info.dgjones.abora.gold.xcvr.Binary2XcvrMaker", "info.dgjones.abora.gold.be.canopy.CanopyCache", "info.dgjones.abora.gold.primtab.PrimPtr2PtrTable", "info.dgjones.abora.gold.proman.DetectorEvent", "info.dgjones.abora.gold.proman.FilledEvent", "info.dgjones.abora.gold.proman.DoneEvent", "info.dgjones.abora.gold.proman.ReleasedEvent", "info.dgjones.abora.gold.proman.RangeFilledEvent", "info.dgjones.abora.gold.proman.GrabbedEvent", "info.dgjones.abora.gold.proman.RevisedEvent", "info.dgjones.abora.gold.x.PrimSpec", "info.dgjones.abora.gold.x.PrimFloatSpec", "info.dgjones.abora.gold.x.PrimPointerSpec", "info.dgjones.abora.gold.x.PrimIntegerSpec", "info.dgjones.abora.gold.set.SHTO", "info.dgjones.abora.gold.collection.steppers.Stepper", "info.dgjones.abora.gold.spaces.integers.AscendingIntegerStepper", "info.dgjones.abora.gold.cross.TupleStepper", "info.dgjones.abora.gold.id.SequenceStepper", "info.dgjones.abora.gold.cross.GenericCrossSimpleRegionStepper", "info.dgjones.abora.gold.cxx.classx.stuff.GrandDataPageStepper", "info.dgjones.abora.gold.id.IDSimpleStepper", "info.dgjones.abora.gold.spaces.integers.IntegerEdgeStepper", "info.dgjones.abora.gold.spaces.cross.BoxProjectionStepper", "info.dgjones.abora.gold.primtab.PrimSetStepper", "info.dgjones.abora.gold.be.ents.MergeBundlesStepper", "info.dgjones.abora.gold.id.IDStepper", "info.dgjones.abora.gold.collection.steppers.TableStepper", "info.dgjones.abora.gold.collection.steppers.ArrayStepper", "info.dgjones.abora.gold.collection.steppers.AscendingArrayStepper", "info.dgjones.abora.gold.spaces.cross.PtrArrayStepper", "info.dgjones.abora.gold.nbacken.EditionStepper", "info.dgjones.abora.gold.collection.steppers.OffsetScruTableStepper", "info.dgjones.abora.gold.cxx.classx.stuff.GrandHashTableStepper", "info.dgjones.abora.gold.collection.steppers.IntegerTableStepper", "info.dgjones.abora.gold.collection.steppers.ITAscendingStepper", "info.dgjones.abora.gold.collection.steppers.ITGenericStepper", "info.dgjones.abora.gold.collection.steppers.ITDescendingStepper", "info.dgjones.abora.gold.nbacken.GrantStepper", "info.dgjones.abora.gold.collection.settable.BucketArrayStepper", "info.dgjones.abora.gold.collection.steppers.OffsetArrayStepper", "info.dgjones.abora.gold.id.RealStepper", "info.dgjones.abora.gold.collection.steppers.HashSetStepper", "info.dgjones.abora.gold.stepper.EmptyStepper", "info.dgjones.abora.gold.cxx.classx.stuff.GrandOverflowStepper", "info.dgjones.abora.gold.stepper.ItemStepper", "info.dgjones.abora.gold.edgeregion.EdgeStepper", "info.dgjones.abora.gold.spaces.integers.IntegerSimpleRegionStepper", "info.dgjones.abora.gold.cxx.classx.stuff.GrandHashSetStepper", "info.dgjones.abora.gold.edgeregion.EdgeSimpleRegionStepper", "info.dgjones.abora.gold.cxx.classx.stuff.GrandNodeStepper", "info.dgjones.abora.gold.settab.SetTableStepper", "info.dgjones.abora.gold.spaces.integers.DescendingIntegerStepper", "info.dgjones.abora.gold.primtab.PrimPtr2PtrTableStepper", "info.dgjones.abora.gold.spaces.cross.BoxStepper", "info.dgjones.abora.gold.primtab.PrimIndexTableStepper", "info.dgjones.abora.gold.spaces.cross.MergeStepper", "info.dgjones.abora.gold.primtab.PrimPtrTableStepper", "info.dgjones.abora.gold.collection.steppers.DisjointRegionStepper", "info.dgjones.abora.gold.fm.support.Thunk", "info.dgjones.abora.gold.cxx.classx.other.EchoThunk", "info.dgjones.abora.gold.appmods.WorksIniter", "info.dgjones.abora.gold.rcmain.ServerLoop", "info.dgjones.abora.gold.rcmain.SelectServerLoop", "info.dgjones.abora.gold.packer.SpareStageSpace", "info.dgjones.abora.gold.hlogger.SwitchLogger", "info.dgjones.abora.gold.xcvr.FakeDisk", "info.dgjones.abora.gold.cxx.classx.other.CommentThunk", "info.dgjones.abora.gold.testing.Tester", "info.dgjones.abora.gold.primtab.PrimIndexTableTester", "info.dgjones.abora.gold.hlogger.LogTester", "info.dgjones.abora.gold.collection.settable.SetTableTester", "info.dgjones.abora.gold.testing.SetTester", "info.dgjones.abora.gold.sheph.ShepherdLockTester", "info.dgjones.abora.gold.primtab.PrimPtrTableTester", "info.dgjones.abora.gold.nkernel.VolumeTester", "info.dgjones.abora.gold.nkernel.WorksTester", "info.dgjones.abora.gold.testing.HelloTester", "info.dgjones.abora.gold.testing.HashTableTester", "info.dgjones.abora.gold.diskman.DiskTester", "info.dgjones.abora.gold.testing.IntegerTableTester", "info.dgjones.abora.gold.spaces.integers.RegionTester", "info.dgjones.abora.gold.spaces.basic.RealTester", "info.dgjones.abora.gold.spaces.basic.SequenceTester", "info.dgjones.abora.gold.cross.CrossTester", "info.dgjones.abora.gold.spaces.basic.IDTester", "info.dgjones.abora.gold.spaces.integers.IntegerRegionTester", "info.dgjones.abora.gold.spaces.basic.FilterTester", "info.dgjones.abora.gold.tabent.TableEntryTester", "info.dgjones.abora.gold.testing.ScruSetTester", "info.dgjones.abora.gold.testing.MuSetTester", "info.dgjones.abora.gold.collection.grand.GrandHashSetTester", "info.dgjones.abora.gold.testing.HashSetTester", "info.dgjones.abora.gold.testing.ImmuSetTester", "info.dgjones.abora.gold.xcvr.ShuffleTester", "info.dgjones.abora.gold.xpp.become.BecomeTester", "info.dgjones.abora.gold.collection.grand.GrandHashTableTester", "info.dgjones.abora.gold.xcvr.DiskIniter", "info.dgjones.abora.gold.cobbler.BootPlan", "info.dgjones.abora.gold.calc.TrackCBlocks", "info.dgjones.abora.gold.cobbler.BootMaker", "info.dgjones.abora.gold.packer.HonestAbePlan", "info.dgjones.abora.gold.fbtest.WorksBootMaker", "info.dgjones.abora.gold.fbtest.BackendBootMaker", "info.dgjones.abora.gold.fbtest.ShepherdBootMaker", "info.dgjones.abora.gold.cobbler.ClearPlan", "info.dgjones.abora.gold.cobbler.FromDiskPlan", "info.dgjones.abora.gold.fbtest.FeWorksBootMaker", "info.dgjones.abora.gold.diskman.Honestly", "info.dgjones.abora.gold.calc.PrintCBlocksTracks", "info.dgjones.abora.gold.rcmain.SetDiskProtocol", "info.dgjones.abora.gold.diskman.HonestAbeIniter", "info.dgjones.abora.gold.rcmain.SetCommProtocol", "info.dgjones.abora.gold.snfinfo.SnarfStatistics", "info.dgjones.abora.gold.snarf.DiskPurgeRate", "info.dgjones.abora.gold.proman.PromiseManager", "info.dgjones.abora.gold.gchooks.SanitationEngineer", "info.dgjones.abora.gold.snarf.Purgeror", "info.dgjones.abora.gold.xpp.converters.Converter", "info.dgjones.abora.gold.spaces.basic.CoordinateSpace", "info.dgjones.abora.gold.spaces.unordered.HeaperSpace", "info.dgjones.abora.gold.spaces.integers.IntegerSpace", "info.dgjones.abora.gold.spaces.basic.BasicSpace", "info.dgjones.abora.gold.tumbler.RealSpace", "info.dgjones.abora.gold.filter.FilterSpace", "info.dgjones.abora.gold.spaces.cross.CrossSpace", "info.dgjones.abora.gold.spaces.cross.GenericCrossSpace", "info.dgjones.abora.gold.tumbler.SequenceSpace", "info.dgjones.abora.gold.spaces.unordered.IDSpace", "info.dgjones.abora.gold.spaces.basic.OrderSpec", "info.dgjones.abora.gold.spaces.basic.ReverseOrder", "info.dgjones.abora.gold.tumbler.IDUpOrder", "info.dgjones.abora.gold.spaces.basic.IntegerUpOrder", "info.dgjones.abora.gold.cross.CrossOrderSpec", "info.dgjones.abora.gold.tumbler.RealUpOrder", "info.dgjones.abora.gold.tumbler.SequenceUpOrder", "info.dgjones.abora.gold.nadmin.FeSession", "info.dgjones.abora.gold.nadmin.FePromiseSession", "info.dgjones.abora.gold.nadmin.DefaultSession", "info.dgjones.abora.gold.nkernel.FeAdminer", "info.dgjones.abora.gold.snarf.DiskManager", "info.dgjones.abora.gold.snarf.TestPacker", "info.dgjones.abora.gold.snarf.CBlockTrackingPacker", "info.dgjones.abora.gold.snarf.FakePacker", "info.dgjones.abora.gold.snarf.SnarfPacker", "info.dgjones.abora.gold.backrec.ResultRecorder", "info.dgjones.abora.gold.backrec.EditionRecorder", "info.dgjones.abora.gold.backrec.IndirectEditionRecorder", "info.dgjones.abora.gold.backrec.DirectEditionRecorder", "info.dgjones.abora.gold.backrec.WorkRecorder", "info.dgjones.abora.gold.backrec.DirectWorkRecorder", "info.dgjones.abora.gold.backrec.IndirectWorkRecorder", "info.dgjones.abora.gold.snarf.SnarfRecord", "info.dgjones.abora.gold.collection.grand.ExponentialHashMap", "info.dgjones.abora.gold.arrange.Arrangement", "info.dgjones.abora.gold.spaces.integers.IntegerArrangement", "info.dgjones.abora.gold.tumbler.ExplicitArrangement", "info.dgjones.abora.gold.xpp.basic.CalcCreator", "info.dgjones.abora.gold.xpp.basic.ActualCalcCreator", "info.dgjones.abora.gold.sysadm.FeArchiver", "info.dgjones.abora.gold.schunk.ChunkCleaner", "info.dgjones.abora.gold.packer.PersistentCleaner", "info.dgjones.abora.gold.xcvr.XnReadStream", "info.dgjones.abora.gold.xcvr.XnBufferedReadStream", "info.dgjones.abora.gold.urdi.ReadArrayStream", "info.dgjones.abora.gold.urdi.ReadMemStream", "info.dgjones.abora.gold.xpp.packages.FakeCategory", "info.dgjones.abora.gold.xpp.packages.FakePackageCategory", "info.dgjones.abora.gold.xpp.converters.CategoryTable", "info.dgjones.abora.gold.xpp.converters.ActualCategoryTable", "info.dgjones.abora.gold.nkernel.FeBundle", "info.dgjones.abora.gold.nkernel.FeElementBundle", "info.dgjones.abora.gold.nkernel.FeArrayBundle", "info.dgjones.abora.gold.nkernel.FePlaceHolderBundle", "info.dgjones.abora.gold.primtab.PrimRemovedObject", "info.dgjones.abora.gold.x.PrimValue", "info.dgjones.abora.gold.x.PrimFloatValue", "info.dgjones.abora.gold.x.PrimIEEE32", "info.dgjones.abora.gold.x.PrimIEEE64", "info.dgjones.abora.gold.x.PrimIntValue", "info.dgjones.abora.gold.be.basic.BeCarrier", "info.dgjones.abora.gold.collection.tables.Pair", "info.dgjones.abora.gold.wrapper.FeWrapperDef", "info.dgjones.abora.gold.wrapper.FeDirectWrapperDef", "info.dgjones.abora.gold.wrapper.FeAbstractWrapperDef", "info.dgjones.abora.gold.wrapper.FeIndirectWrapperDef", "info.dgjones.abora.gold.lock.Scrambler", "info.dgjones.abora.gold.lock.NoScrambler", "info.dgjones.abora.gold.cobbler.Cookbook", "info.dgjones.abora.gold.cobbler.ActualCookbook", "info.dgjones.abora.gold.lock.Encrypter", "info.dgjones.abora.gold.lock.NoEncrypter", "info.dgjones.abora.gold.edge.EdgeManager", "info.dgjones.abora.gold.tumbler.SequenceManager", "info.dgjones.abora.gold.tumbler.RealManager", "info.dgjones.abora.gold.edgeregion.TransitionEdge", "info.dgjones.abora.gold.tumbler.RealEdge", "info.dgjones.abora.gold.tumbler.AfterReal", "info.dgjones.abora.gold.tumbler.BeforeReal", "info.dgjones.abora.gold.tumbler.SequenceEdge", "info.dgjones.abora.gold.tumbler.AfterSequence", "info.dgjones.abora.gold.tumbler.BeforeSequence", "info.dgjones.abora.gold.tumbler.BeforeSequencePrefix", "info.dgjones.abora.gold.tabtool.PrimeSizeProvider", "info.dgjones.abora.gold.tabtool.LPPrimeSizeProvider", "info.dgjones.abora.gold.primtab.PrimPtrTable", "info.dgjones.abora.gold.wrapper.FeWrapper", "info.dgjones.abora.gold.nadmin.FeClubDescription", "info.dgjones.abora.gold.nadmin.FeLockSmith", "info.dgjones.abora.gold.nadmin.FeChallengeLockSmith", "info.dgjones.abora.gold.nadmin.FeMatchLockSmith", "info.dgjones.abora.gold.nadmin.FeWallLockSmith", "info.dgjones.abora.gold.nadmin.FeBooLockSmith", "info.dgjones.abora.gold.nadmin.FeMultiLockSmith", "info.dgjones.abora.gold.wrapper.FeSet", "info.dgjones.abora.gold.nlinks.FeHyperLink", "info.dgjones.abora.gold.wrapper.FeWorkSet", "info.dgjones.abora.gold.wrapper.FeText", "info.dgjones.abora.gold.nlinks.FePath", "info.dgjones.abora.gold.nlinks.FeHyperRef", "info.dgjones.abora.gold.nlinks.FeSingleRef", "info.dgjones.abora.gold.nlinks.FeMultiRef", "info.dgjones.abora.gold.canopy.Heaper2UInt32Cache", "info.dgjones.abora.gold.spaces.basic.Mapping", "info.dgjones.abora.gold.spaces.EmptyMapping", "info.dgjones.abora.gold.spaces.ConstantMapping", "info.dgjones.abora.gold.spaces.CompositeMapping", "info.dgjones.abora.gold.spaces.basic.Dsp", "info.dgjones.abora.gold.spaces.integers.IntegerMapping", "info.dgjones.abora.gold.cross.CrossMapping", "info.dgjones.abora.gold.cross.GenericCrossDsp", "info.dgjones.abora.gold.spaces.unordered.IdentityDsp", "info.dgjones.abora.gold.filter.RealDsp", "info.dgjones.abora.gold.spaces.unordered.HeaperDsp", "info.dgjones.abora.gold.filter.FilterDsp", "info.dgjones.abora.gold.spaces.unordered.IDDsp", "info.dgjones.abora.gold.tumbler.SequenceMapping", "info.dgjones.abora.gold.spaces.basic.SimpleMapping", "info.dgjones.abora.gold.nkernel.FeRangeElement", "info.dgjones.abora.gold.nkernel.FePlaceHolder", "info.dgjones.abora.gold.nkernel.FeActualPlaceHolder", "info.dgjones.abora.gold.nkernel.FeVirtualPlaceHolder", "info.dgjones.abora.gold.nkernel.FeGrandPlaceHolder", "info.dgjones.abora.gold.nkernel.FeLabel", "info.dgjones.abora.gold.nkernel.FeIDHolder", "info.dgjones.abora.gold.nkernel.FeDataHolder", "info.dgjones.abora.gold.nkernel.FeActualDataHolder", "info.dgjones.abora.gold.nkernel.FeVirtualDataHolder", "info.dgjones.abora.gold.nkernel.FeEdition", "info.dgjones.abora.gold.nkernel.FeWork", "info.dgjones.abora.gold.nkernel.FeClub", "info.dgjones.abora.gold.be.canopy.PropFinder", "info.dgjones.abora.gold.be.canopy.OpenPropFinder", "info.dgjones.abora.gold.be.canopy.SensorPropFinder", "info.dgjones.abora.gold.be.canopy.AbstractRecorderFinder", "info.dgjones.abora.gold.be.canopy.SimpleRecorderFinder", "info.dgjones.abora.gold.be.canopy.ResultRecorderPFinder", "info.dgjones.abora.gold.be.canopy.ContainedEditionRecorderEFinder", "info.dgjones.abora.gold.be.canopy.OriginalResultRecorderEFinder", "info.dgjones.abora.gold.be.canopy.AnyRecorderFinder", "info.dgjones.abora.gold.be.canopy.AnyRecorderPFinder", "info.dgjones.abora.gold.be.canopy.AnyRecorderEFinder", "info.dgjones.abora.gold.be.canopy.CumulativeRecorderFinder", "info.dgjones.abora.gold.be.canopy.PartialityFinder", "info.dgjones.abora.gold.be.canopy.ClosedPropFinder", "info.dgjones.abora.gold.be.canopy.BertPropFinder", "info.dgjones.abora.gold.be.canopy.BackfollowFinder", "info.dgjones.abora.gold.be.canopy.SensorFinder", "info.dgjones.abora.gold.be.canopy.CannotPartializeFinder", "info.dgjones.abora.gold.be.canopy.BackfollowPFinder", "info.dgjones.abora.gold.xpp.basic.Category", "info.dgjones.abora.gold.xcvr.TransferSpecialist", "info.dgjones.abora.gold.xcvr.DiskSpecialist", "info.dgjones.abora.gold.snarf.DiskCountSpecialist", "info.dgjones.abora.gold.xcvr.TransferGeneralist", "info.dgjones.abora.gold.xpp.basic.DeletedHeaper", "info.dgjones.abora.gold.gchooks.RepairEngineer", "info.dgjones.abora.gold.purging.LiberalPurgeror", "info.dgjones.abora.gold.proman.Portal", "info.dgjones.abora.gold.proman.PacketPortal", "info.dgjones.abora.gold.proman.PairPortal", "info.dgjones.abora.gold.cache.InstanceCache", "info.dgjones.abora.gold.be.ents.HistoryCrum", "info.dgjones.abora.gold.be.ents.HBottomCrum", "info.dgjones.abora.gold.be.ents.HUpperCrum", "info.dgjones.abora.gold.collection.steppers.Accumulator", "info.dgjones.abora.gold.spaces.integers.IntegerEdgeAccumulator", "info.dgjones.abora.gold.aspire.PtrArrayAccumulator", "info.dgjones.abora.gold.collection.sets.UnionRecruiter", "info.dgjones.abora.gold.cross.BoxAccumulator", "info.dgjones.abora.gold.collection.sets.SetAccumulator", "info.dgjones.abora.gold.edgeregion.EdgeAccumulator", "info.dgjones.abora.gold.collection.steppers.TableAccumulator", "info.dgjones.abora.gold.collection.steppers.ArrayAccumulator", "info.dgjones.abora.gold.proman.RequestHandler", "info.dgjones.abora.gold.proman.HHHBHandler", "info.dgjones.abora.gold.proman.HHHHHHHHandler", "info.dgjones.abora.gold.proman.BHHHHandler", "info.dgjones.abora.gold.proman.ExampleHIHHandler", "info.dgjones.abora.gold.proman.BHHHandler", "info.dgjones.abora.gold.proman.HHHHHHandler", "info.dgjones.abora.gold.proman.VHHHHandler", "info.dgjones.abora.gold.proman.HHHandler", "info.dgjones.abora.gold.proman.HHHHHHHandler", "info.dgjones.abora.gold.proman.VHHandler", "info.dgjones.abora.gold.proman.SpecialHandler", "info.dgjones.abora.gold.proman.BHHandler", "info.dgjones.abora.gold.proman.VHBHandler", "info.dgjones.abora.gold.proman.VHHHandler", "info.dgjones.abora.gold.proman.HHandler", "info.dgjones.abora.gold.proman.HHHHHandler", "info.dgjones.abora.gold.proman.HHHHandler", "info.dgjones.abora.gold.proman.HHBHandler", "info.dgjones.abora.gold.proman.VHHHHHandler", "info.dgjones.abora.gold.proman.VHHHHHHandler", "info.dgjones.abora.gold.xcvr.Xmtr", "info.dgjones.abora.gold.xcvr.SpecialistXmtr", "info.dgjones.abora.gold.xcvr.TextyXmtr", "info.dgjones.abora.gold.xcvr.Binary2Xmtr", "info.dgjones.abora.gold.snarf.CBlockTracker", "info.dgjones.abora.gold.cache.CacheManager", "info.dgjones.abora.gold.snfinfo.SpecialistRcvrJig", "info.dgjones.abora.gold.collection.settable.SetTable", "info.dgjones.abora.gold.filter.RegionDelta", "info.dgjones.abora.gold.xcvr.XnWriteStream", "info.dgjones.abora.gold.urdi.WriteMemStream", "info.dgjones.abora.gold.urdi.CountStream", "info.dgjones.abora.gold.urdi.HashStream", "info.dgjones.abora.gold.urdi.WriteArrayStream", "info.dgjones.abora.gold.xcvr.XnBufferedWriteStream", "info.dgjones.abora.gold.urdi.WriteVariableArrayStream", "info.dgjones.abora.gold.spaces.basic.XnRegion", "info.dgjones.abora.gold.filter.Filter", "info.dgjones.abora.gold.filter.NotSubsetFilter", "info.dgjones.abora.gold.filter.ClosedFilter", "info.dgjones.abora.gold.filter.SupersetFilter", "info.dgjones.abora.gold.filter.SubsetFilter", "info.dgjones.abora.gold.filter.AndFilter", "info.dgjones.abora.gold.filter.OrFilter", "info.dgjones.abora.gold.filter.NotSupersetFilter", "info.dgjones.abora.gold.filter.OpenFilter", "info.dgjones.abora.gold.tumbler.RealRegion", "info.dgjones.abora.gold.tumbler.SequenceRegion", "info.dgjones.abora.gold.spaces.integers.IntegerRegion", "info.dgjones.abora.gold.id.IDRegion", "info.dgjones.abora.gold.spaces.cross.CrossRegion", "info.dgjones.abora.gold.spaces.cross.GenericCrossRegion", "info.dgjones.abora.gold.spaces.unordered.SetRegion", "info.dgjones.abora.gold.hspace.HeaperRegion", "info.dgjones.abora.gold.collection.sets.ScruSet", "info.dgjones.abora.gold.collection.sets.MuSet", "info.dgjones.abora.gold.collection.grand.GrandHashSet", "info.dgjones.abora.gold.collection.sets.HashSet", "info.dgjones.abora.gold.collection.sets.ActualHashSet", "info.dgjones.abora.gold.collection.sets.ImmuSet", "info.dgjones.abora.gold.collection.sets.TinyImmuSet", "info.dgjones.abora.gold.collection.sets.ImmuSetOnMu", "info.dgjones.abora.gold.collection.sets.EmptyImmuSet", "info.dgjones.abora.gold.snarf.SnarfHandler", "info.dgjones.abora.gold.wparray.XnExecutor", "info.dgjones.abora.gold.diskman.Cattleman", "info.dgjones.abora.gold.brange1.FillDetectorExecutor", "info.dgjones.abora.gold.brange2.BeWorkLockExecutor", "info.dgjones.abora.gold.brange2.RevisionWatcherExecutor", "info.dgjones.abora.gold.brange3.BeEditionDetectorExecutor", "info.dgjones.abora.gold.primtab.PrimSetExecutor", "info.dgjones.abora.gold.gchooks.DeleteExecutor", "info.dgjones.abora.gold.nkernel.StatusDetectorExecutor", "info.dgjones.abora.gold.gchooks.CloseExecutor", "info.dgjones.abora.gold.primtab.PrimPtrTableExecutor", "info.dgjones.abora.gold.nkernel.RevisionDetectorExecutor", "info.dgjones.abora.gold.xcvr.Recipe", "info.dgjones.abora.gold.cxx.classx.comm.StubRecipe", "info.dgjones.abora.gold.cxx.otherclass.CopyRecipe", "info.dgjones.abora.gold.cxx.classx.comm.CategoryRecipe", "info.dgjones.abora.gold.rcmain.MainDummy", "info.dgjones.abora.gold.chameleon.Chameleon", "info.dgjones.abora.gold.chameleon.DeadMoth", "info.dgjones.abora.gold.cxx.classx.stuff.DeadButterfly", "info.dgjones.abora.gold.chameleon.Moth", "info.dgjones.abora.gold.chameleon.Butterfly", "info.dgjones.abora.gold.chameleon.GoldButterfly", "info.dgjones.abora.gold.chameleon.IronButterfly", "info.dgjones.abora.gold.chameleon.LeadButterfly", "info.dgjones.abora.gold.xcvr.Rcvr", "info.dgjones.abora.gold.xcvr.SpecialistRcvr", "info.dgjones.abora.gold.xcvr.TextyRcvr", "info.dgjones.abora.gold.xcvr.Binary2Rcvr", "info.dgjones.abora.gold.tokens.TokenSource", "info.dgjones.abora.gold.snarf.Abraham", "info.dgjones.abora.gold.be.ents.OPart", "info.dgjones.abora.gold.be.ents.Loaf", "info.dgjones.abora.gold.be.ents.OExpandingLoaf", "info.dgjones.abora.gold.be.ents.RegionLoaf", "info.dgjones.abora.gold.be.ents.OPartialLoaf", "info.dgjones.abora.gold.be.ents.OVirtualLoaf", "info.dgjones.abora.gold.be.ents.InnerLoaf", "info.dgjones.abora.gold.be.ents.SplitLoaf", "info.dgjones.abora.gold.be.ents.DspLoaf", "info.dgjones.abora.gold.be.ents.OrglRoot", "info.dgjones.abora.gold.be.ents.ActualOrglRoot", "info.dgjones.abora.gold.be.ents.EmptyOrglRoot", "info.dgjones.abora.gold.collection.grand.GrandOverflow", "info.dgjones.abora.gold.tclude.TrailBlazer", "info.dgjones.abora.gold.collection.grand.GrandDataPage", "info.dgjones.abora.gold.collection.grand.GrandEntry", "info.dgjones.abora.gold.collection.grand.GrandTableEntry", "info.dgjones.abora.gold.collection.grand.GrandSetEntry", "info.dgjones.abora.gold.be.ents.SharedData", "info.dgjones.abora.gold.snarf.Turtle", "info.dgjones.abora.gold.snarf.SimpleTurtle", "info.dgjones.abora.gold.snarf.MockTurtle", "info.dgjones.abora.gold.turtle.AgendaItem", "info.dgjones.abora.gold.turtle.NorthRecorderChecker", "info.dgjones.abora.gold.grantab.GrandNodeDoubler", "info.dgjones.abora.gold.turtle.Agenda", "info.dgjones.abora.gold.grantab.GrandNodeReinserter", "info.dgjones.abora.gold.brange2.UpdateTransitiveSuperClubIDs", "info.dgjones.abora.gold.turtle.PropChanger", "info.dgjones.abora.gold.turtle.ActualPropChanger", "info.dgjones.abora.gold.turtle.RecorderHoister", "info.dgjones.abora.gold.turtle.HeightChanger", "info.dgjones.abora.gold.turtle.Matcher", "info.dgjones.abora.gold.turtle.Sequencer", "info.dgjones.abora.gold.brange2.UpdateTransitiveMemberIDs", "info.dgjones.abora.gold.turtle.SouthRecorderChecker", "info.dgjones.abora.gold.turtle.RecorderTrigger", "info.dgjones.abora.gold.traces.DagWood", "info.dgjones.abora.gold.snarf.Pumpkin", "info.dgjones.abora.gold.be.basic.BeGrandMap", "info.dgjones.abora.gold.counter.MultiCounter", "info.dgjones.abora.gold.traces.BranchDescription", "info.dgjones.abora.gold.traces.DagBranch", "info.dgjones.abora.gold.traces.TreeBranch", "info.dgjones.abora.gold.traces.RootBranch", "info.dgjones.abora.gold.be.canopy.CanopyCrum", "info.dgjones.abora.gold.be.canopy.SensorCrum", "info.dgjones.abora.gold.be.canopy.BertCrum", "info.dgjones.abora.gold.snarf.PairFlock", "info.dgjones.abora.gold.fossil.RecorderFossil", "info.dgjones.abora.gold.fossil.WorkRecorderFossil", "info.dgjones.abora.gold.fossil.IndirectWorkRecorderFossil", "info.dgjones.abora.gold.fossil.DirectWorkRecorderFossil", "info.dgjones.abora.gold.fossil.EditionRecorderFossil", "info.dgjones.abora.gold.fossil.IndirectEditionRecorderFossil", "info.dgjones.abora.gold.fossil.DirectEditionRecorderFossil", "info.dgjones.abora.gold.counter.Counter", "info.dgjones.abora.gold.counter.BatchCounter", "info.dgjones.abora.gold.counter.SingleCounter", "info.dgjones.abora.gold.sheph.ShepherdLocked", "info.dgjones.abora.gold.collection.grand.GrandNode", "info.dgjones.abora.gold.snarf.DoublingFlock", "info.dgjones.abora.gold.be.basic.BeRangeElement", "info.dgjones.abora.gold.be.basic.BeLabel", "info.dgjones.abora.gold.be.basic.BeEdition", "info.dgjones.abora.gold.be.basic.BeDataHolder", "info.dgjones.abora.gold.be.basic.BeWork", "info.dgjones.abora.gold.be.basic.BeClub", "info.dgjones.abora.gold.be.basic.BeIDHolder", "info.dgjones.abora.gold.be.basic.BePlaceHolder", "info.dgjones.abora.gold.be.ents.Ent", "info.dgjones.abora.gold.cobbler.Connection", "info.dgjones.abora.gold.cobbler.NestedConnection", "info.dgjones.abora.gold.cobbler.DiskConnection", "info.dgjones.abora.gold.cobbler.DirectConnection", "info.dgjones.abora.gold.nkernel.FeKeyMaster", "info.dgjones.abora.gold.negoti8.ProtocolItem", "info.dgjones.abora.gold.nkernel.FeServer", "info.dgjones.abora.gold.lock.EncrypterMaker", "info.dgjones.abora.gold.stacker.StackExaminer", "info.dgjones.abora.gold.primtab.PrimSet", "info.dgjones.abora.gold.java.missing.ShepherdStub", "info.dgjones.abora.gold.collection.basic.IEEE32Array", "info.dgjones.abora.gold.collection.basic.IEEE64Array", "info.dgjones.abora.gold.collection.basic.Int32Array", "info.dgjones.abora.gold.collection.basic.Int8Array", "info.dgjones.abora.gold.collection.basic.IntegerVarArray", "info.dgjones.abora.gold.collection.basic.PrimArray", "info.dgjones.abora.gold.collection.basic.PrimDataArray", "info.dgjones.abora.gold.collection.basic.PrimFloatArray", "info.dgjones.abora.gold.collection.basic.PrimIntArray", "info.dgjones.abora.gold.collection.basic.PrimIntegerArray", "info.dgjones.abora.gold.collection.basic.PtrArray", "info.dgjones.abora.gold.collection.basic.SharedPtrArray", "info.dgjones.abora.gold.collection.basic.UInt32Array", "info.dgjones.abora.gold.collection.basic.UInt8Array", "info.dgjones.abora.gold.collection.basic.WeakPtrArray"};
/*

Generated during transformation: RecordHeaperHierarchy
*/
}
public static String[][] initTimeNonInheritedDependencies() {
	return new String[][] 
	{
	{"info.dgjones.abora.gold.collection.tables.MuTable", "info.dgjones.abora.gold.collection.tables.IntegerTable", "info.dgjones.abora.gold.collection.tables.HashTable", "info.dgjones.abora.gold.spaces.integers.IntegerSpace", }, 
	{"info.dgjones.abora.gold.collection.grand.GrandHashTable", "info.dgjones.abora.gold.collection.grand.ExponentialHashMap", }, 
	{"info.dgjones.abora.gold.collection.tables.HashTable", "info.dgjones.abora.gold.tabtool.LPPrimeSizeProvider", "info.dgjones.abora.gold.collection.sets.ImmuSet", }, 
	{"info.dgjones.abora.gold.wrapper.FeWrapperSpec", "info.dgjones.abora.gold.tumbler.SequenceSpace", "info.dgjones.abora.gold.collection.tables.MuTable", }, 
	{"info.dgjones.abora.gold.tumbler.Sequence", }, 
	{"info.dgjones.abora.gold.rcmain.ServerLoop", "info.dgjones.abora.gold.collection.sets.MuSet", }, 
	{"info.dgjones.abora.gold.cobbler.BootPlan", "info.dgjones.abora.gold.xcvr.Recipe", }, 
	{"info.dgjones.abora.gold.fbtest.BackendBootMaker", "info.dgjones.abora.gold.xcvr.Recipe", }, 
	{"info.dgjones.abora.gold.xpp.converters.Converter", "info.dgjones.abora.gold.xpp.converters.CategoryTable", }, 
	{"info.dgjones.abora.gold.tumbler.RealSpace", "info.dgjones.abora.gold.x.PrimSpec", }, 
	{"info.dgjones.abora.gold.tumbler.SequenceSpace", "info.dgjones.abora.gold.tumbler.Sequence", }, 
	{"info.dgjones.abora.gold.lock.Scrambler", "info.dgjones.abora.gold.tumbler.SequenceSpace", "info.dgjones.abora.gold.collection.tables.MuTable", }, 
	{"info.dgjones.abora.gold.lock.Encrypter", "info.dgjones.abora.gold.tumbler.SequenceSpace", "info.dgjones.abora.gold.collection.tables.MuTable", }, 
	{"info.dgjones.abora.gold.tabtool.PrimeSizeProvider", "info.dgjones.abora.gold.tabtool.LPPrimeSizeProvider", }, 
	{"info.dgjones.abora.gold.tabtool.LPPrimeSizeProvider", }, 
	{"info.dgjones.abora.gold.canopy.Heaper2UInt32Cache", "info.dgjones.abora.gold.collection.basic.PrimArray", "info.dgjones.abora.gold.tabtool.PrimeSizeProvider", }, 
	{"info.dgjones.abora.gold.tumbler.RealRegion", "info.dgjones.abora.gold.edge.EdgeManager", }, 
	{"info.dgjones.abora.gold.tumbler.SequenceRegion", "info.dgjones.abora.gold.collection.basic.PtrArray", "info.dgjones.abora.gold.tumbler.SequenceSpace", "info.dgjones.abora.gold.edge.EdgeManager", "info.dgjones.abora.gold.tumbler.Sequence", }, 
	{"info.dgjones.abora.gold.spaces.integers.IntegerRegion", }, 
	{"info.dgjones.abora.gold.id.IDRegion", "info.dgjones.abora.gold.x.PrimSpec", "info.dgjones.abora.gold.spaces.integers.IntegerRegion", }, 
	{"info.dgjones.abora.gold.collection.sets.MuSet", "info.dgjones.abora.gold.collection.sets.ActualHashSet", }, 
	{"info.dgjones.abora.gold.collection.grand.GrandHashSet", "info.dgjones.abora.gold.collection.grand.ExponentialHashMap", }, 
	{"info.dgjones.abora.gold.collection.sets.ActualHashSet", "info.dgjones.abora.gold.tabtool.LPPrimeSizeProvider", }, 
	{"info.dgjones.abora.gold.collection.sets.ImmuSet", "info.dgjones.abora.gold.collection.steppers.Stepper", }, 
	{"info.dgjones.abora.gold.be.canopy.CanopyCrum", "info.dgjones.abora.gold.canopy.Heaper2UInt32Cache", }, };
/*

Generated during transformation: RecordHeaperHierarchy
*/
}
}
