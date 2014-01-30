/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.wrapper;

import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.collection.tables.MuTable;
import info.dgjones.abora.gold.collection.tables.ScruTable;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.FeDirectWrapperChecker;
import info.dgjones.abora.gold.java.missing.FeDirectWrapperMaker;
import info.dgjones.abora.gold.java.missing.FeIndirectWrapperChecker;
import info.dgjones.abora.gold.java.missing.FeIndirectWrapperMaker;
import info.dgjones.abora.gold.java.missing.FeWrapperSpecHolder;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.cross.CrossRegion;
import info.dgjones.abora.gold.spaces.cross.Tuple;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.wrapper.FeAbstractWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperDef;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Handles wrapping, certification, and filtering for a wrapper type and its subtypes (if
 * there are any)
 */
public class FeWrapperSpec extends Heaper {

	protected FeWrapperDef myDef;
	protected CrossRegion myEndorsements;
	protected Filter myFilter;
	protected FeAbstractWrapperSpec mySuperSpec;
	protected static MuTable TheWrapperDefs;
	protected static MuTable TheWrapperEndorsements;
	protected static MuTable TheWrappersFromEndorsements;
	protected static MuTable TheWrapperSpecs;
/*
udanax-top.st:25888:
Heaper subclass: #FeWrapperSpec
	instanceVariableNames: '
		myDef {FeWrapperDef}
		myEndorsements {CrossRegion}
		myFilter {Filter}
		mySuperSpec {FeAbstractWrapperSpec | NULL}'
	classVariableNames: '
		TheWrapperDefs {MuTable of: Tumbler with: FeWrapperDef} 
		TheWrapperEndorsements {MuTable of: Tumbler with: CrossRegion} 
		TheWrappersFromEndorsements {MuTable of: Tuple with: FeWrapperSpec} 
		TheWrapperSpecs {MuTable of: Tumbler with: FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:25900:
FeWrapperSpec comment:
'Handles wrapping, certification, and filtering for a wrapper type and its subtypes (if there are any)'!
*/
/*
udanax-top.st:25902:
(FeWrapperSpec getOrMakeCxxClassDescription)
	friends:
'/- friends for class FeWrapperSpec -/
friend class FeWrapper;
';
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #EQ; yourself)!
*/
/*
udanax-top.st:26011:
FeWrapperSpec class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:26014:
(FeWrapperSpec getOrMakeCxxClassDescription)
	friends:
'/- friends for class FeWrapperSpec -/
friend class FeWrapper;
';
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeWrapperSpec.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Whether the Edition passes the invariants of this type so that it could be certified.
 * Always checks the actual contents and endorses if they are acceptable.
 */
public boolean certify(FeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:25911:FeWrapperSpec methodsFor: 'accessing'!
{BooleanVar} certify: edition {FeEdition}
	"Whether the Edition passes the invariants of this type so that it could be certified. Always checks the actual contents and endorses if they are acceptable."
	self subclassResponsibility!
*/
}
/**
 * A filter which selects for Editions which have been endorsed as belonging to this type.
 */
public Filter filter() {
	if (myFilter == null) {
		myFilter = (Filter) ((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementFilterSpace().emptyRegion();
	}
	return myFilter;
/*
udanax-top.st:25915:FeWrapperSpec methodsFor: 'accessing'!
{Filter CLIENT} filter
	"A filter which selects for Editions which have been endorsed as belonging to this type."
	
	myFilter == NULL ifTrue:
		[myFilter := CurrentGrandMap fluidGet endorsementFilterSpace emptyRegion cast: Filter].
	^myFilter!
*/
}
/**
 * Whether an Edition is already endorsed as being of this type. Equivalent to
 * this->filter ()->match (edition->endorsements ())
 */
public boolean isCertified(FeEdition edition) {
	return filter().match(edition.endorsements());
/*
udanax-top.st:25922:FeWrapperSpec methodsFor: 'accessing'!
{BooleanVar} isCertified: edition {FeEdition}
	"Whether an Edition is already endorsed as being of this type. Equivalent to
		this->filter ()->match (edition->endorsements ())"
	^self filter match: edition endorsements!
*/
}
/**
 * The name for this type
 */
public Sequence name() {
	return myDef.name();
/*
udanax-top.st:25927:FeWrapperSpec methodsFor: 'accessing'!
{Sequence CLIENT} name
	"The name for this type"
	
	^myDef name!
*/
}
/**
 * The Edition wrapped with my type of Wrapper. If it does not have endorsements, will
 * attempt to certify. Blasts if there is more than one valid wrapping.
 */
public FeWrapper wrap(FeEdition edition) {
	FeWrapper result;
	result = fetchWrap(edition);
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.CANNOT_WRAP);
	}
	return result;
/*
udanax-top.st:25932:FeWrapperSpec methodsFor: 'accessing'!
{FeWrapper CLIENT} wrap: edition {FeEdition}
	"The Edition wrapped with my type of Wrapper. If it does not have endorsements, will attempt to certify. Blasts if there is more than one valid wrapping."
	
	| result {FeWrapper} |
	result := self fetchWrap: edition.
	result == NULL ifTrue:
		[Heaper BLAST: #CannotWrap].
	^result!
*/
}
public FeWrapper fetchWrap(FeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:25943:FeWrapperSpec methodsFor: 'vulnerable'!
{FeWrapper | NULL} fetchWrap: edition {FeEdition}
	self subclassResponsibility!
*/
}
/**
 * Whether this is the same as or a kind of the other spec
 */
public boolean isSubSpecOf(FeWrapperSpec other) {
	return this == other || ((other instanceof FeAbstractWrapperSpec) && (fetchSuperSpec() != null && (fetchSuperSpec().isSubSpecOf(other))));
/*
udanax-top.st:25947:FeWrapperSpec methodsFor: 'vulnerable'!
{BooleanVar} isSubSpecOf: other {FeWrapperSpec}
	"Whether this is the same as or a kind of the other spec"
	^self == other
		or: [(other isKindOf: FeAbstractWrapperSpec)
			and: [self fetchSuperSpec ~~ NULL
			and: [self fetchSuperSpec isSubSpecOf: other]]]!
*/
}
/**
 * Add some more endorsements to filter for
 */
public void addToFilter(CrossRegion endorsements) {
	myFilter = (Filter) (filter().unionWith((((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementFilterSpace().anyFilter(endorsements))));
/*
udanax-top.st:25956:FeWrapperSpec methodsFor: 'protected:'!
{void} addToFilter: endorsements {CrossRegion}
	"Add some more endorsements to filter for"
	
	myFilter := (self filter
		unionWith: (CurrentGrandMap fluidGet endorsementFilterSpace
			anyFilter: endorsements)) cast: Filter!
*/
}
public FeWrapperDef def() {
	return myDef;
/*
udanax-top.st:25963:FeWrapperSpec methodsFor: 'protected:'!
{FeWrapperDef} def
	^myDef!
*/
}
/**
 * The immediate supertype, or NULL if this is the generic Wrapper type
 */
public FeAbstractWrapperSpec fetchSuperSpec() {
	return mySuperSpec;
/*
udanax-top.st:25966:FeWrapperSpec methodsFor: 'protected:'!
{FeAbstractWrapperSpec | NULL} fetchSuperSpec
	"The immediate supertype, or NULL if this is the generic Wrapper type"
	^mySuperSpec!
*/
}
/**
 * Do the required setup for this spec in the context of a table of all known specs
 */
public void setup() {
	if (mySuperSpec == null && (myDef.fetchSuperDefName() != null)) {
		CrossRegion end;
		mySuperSpec = (FeAbstractWrapperSpec) (FeWrapperSpec.get(myDef.fetchSuperDefName()));
		myDef.setSpec(this);
		end = FeWrapperSpec.getEndorsements(name());
		myEndorsements = (CrossRegion) (endorsements().unionWith(end));
		addToFilter(end);
	}
/*
udanax-top.st:25970:FeWrapperSpec methodsFor: 'protected:'!
{void} setup
	"Do the required setup for this spec in the context of a table of all known specs"
	
	(mySuperSpec == NULL and: [myDef fetchSuperDefName ~~ NULL]) ifTrue:
		[ | end {CrossRegion} |
		mySuperSpec := (FeWrapperSpec get: myDef fetchSuperDefName)
			cast: FeAbstractWrapperSpec.
		myDef setSpec: self.
		end := FeWrapperSpec getEndorsements: self name.
		myEndorsements := (self endorsements unionWith: end) cast: CrossRegion.
		self addToFilter: end].!
*/
}
public FeWrapperSpec(FeWrapperDef def) {
	super();
	myDef = def;
	myEndorsements = null;
	myFilter = null;
	mySuperSpec = null;
/*
udanax-top.st:25984:FeWrapperSpec methodsFor: 'create'!
create: def {FeWrapperDef}
	super create.
	myDef := def.
	myEndorsements := NULL.
	myFilter := NULL.
	mySuperSpec := NULL.!
*/
}
/**
 * Endorse the Edition as being of this type. Blasts if this is an abstract type.
 * Should only be called from the code implementing the type, or code which it trusts. We may
 * eventually add a system to enforce this.
 */
public void endorse(FeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:25993:FeWrapperSpec methodsFor: 'for wrappers only'!
{void} endorse: edition {FeEdition}
	"Endorse the Edition as being of this type. Blasts if this is an abstract type.
	Should only be called from the code implementing the type, or code which it trusts. We may eventually add a system to enforce this."
	self subclassResponsibility!
*/
}
public CrossRegion endorsements() {
	if (myEndorsements == null) {
		myEndorsements = (CrossRegion) ((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementSpace().emptyRegion();
	}
	return myEndorsements;
/*
udanax-top.st:25998:FeWrapperSpec methodsFor: 'for wrappers only'!
{CrossRegion} endorsements
	myEndorsements == NULL ifTrue:
		[myEndorsements := CurrentGrandMap fluidGet endorsementSpace emptyRegion cast: CrossRegion].
	^myEndorsements!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:26006:FeWrapperSpec methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:26008:FeWrapperSpec methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
/**
 * From a dynamic initializer, register an abstract Wrapper type
 */
public static void ABSTRACTWRAPPER(String wrapperName, String superName, String className) {
	FeWrapperSpec.registerAbstract(wrapperName, superName, new FeWrapperSpecHolder(AboraSupport.findCategory(className), SET_SPEC_));
/*
udanax-top.st:26023:FeWrapperSpec class methodsFor: 'smalltalk: macros:'!
ABSTRACTWRAPPER: wrapperName {char star} with: superName {char star | NULL} with: className {Symbol}
	"From a dynamic initializer, register an abstract Wrapper type"
	self REQUIRES: Sequence.
	self REQUIRES: FeWrapperSpec.
	FeWrapperSpec registerAbstract: wrapperName
		with: superName
		with: ((Smalltalk at: className) pointerToStaticMember: #setSpec:)!
*/
}
/**
 * From a dynamic initializer, register an abstract Wrapper type
 */
public static void DIRECTWRAPPER(String wrapperName, String superName, String className) {
	FeWrapperSpec.registerDirect(wrapperName, superName, new FeDirectWrapperMaker(AboraSupport.findCategory(className), MAKE_WRAPPER_), new FeDirectWrapperChecker(AboraSupport.findCategory(className), CHECK_), new FeWrapperSpecHolder(AboraSupport.findCategory(className), SET_SPEC_));
/*
udanax-top.st:26031:FeWrapperSpec class methodsFor: 'smalltalk: macros:'!
DIRECTWRAPPER: wrapperName {char star} with: superName {char star} with: className {Symbol}
	"From a dynamic initializer, register an abstract Wrapper type"
	self REQUIRES: Sequence.
	self REQUIRES: FeWrapperSpec.
	FeWrapperSpec registerDirect: wrapperName
		with: superName
		with: ((Smalltalk at: className) pointerToStaticMember: #makeWrapper:)
		with: ((Smalltalk at: className) pointerToStaticMember: #check:)
		with: ((Smalltalk at: className) pointerToStaticMember: #setSpec:)!
*/
}
/**
 * From a dynamic initializer, register an abstract Wrapper type
 */
public static void INDIRECTWRAPPER(String wrapperName, String superName, String innerName, String className) {
	FeWrapperSpec.registerIndirect(wrapperName, superName, innerName, new FeIndirectWrapperMaker(AboraSupport.findCategory(className), MAKE_WRAPPER_), new FeIndirectWrapperChecker(AboraSupport.findCategory(className), CHECK_), new FeWrapperSpecHolder(AboraSupport.findCategory(className), SET_SPEC_));
/*
udanax-top.st:26041:FeWrapperSpec class methodsFor: 'smalltalk: macros:'!
INDIRECTWRAPPER: wrapperName {char star}
	with: superName {char star | NULL}
	with: innerName {char star | NULL}
	with: className {Symbol}
	"From a dynamic initializer, register an abstract Wrapper type"
	self REQUIRES: Sequence.
	self REQUIRES: FeWrapperSpec.
	FeWrapperSpec registerIndirect: wrapperName
		with: superName
		with: innerName
		with: ((Smalltalk at: className) pointerToStaticMember: #makeWrapper:)
		with: ((Smalltalk at: className) pointerToStaticMember: #check:)
		with: ((Smalltalk at: className) pointerToStaticMember: #setSpec:)!
*/
}
public static void registerAbstract(String wrapperName, String superName, FeWrapperSpecHolder holder) {
	Sequence wrapper;
	Sequence superWrapper;
	wrapper = Sequence.string(wrapperName);
	if (superName == null) {
		superWrapper = null;
	}
	else {
		superWrapper = Sequence.string(superName);
	}
	TheWrapperDefs.introduce(wrapper, (FeWrapperDef.abstractx(wrapper, superWrapper, holder)));
/*
udanax-top.st:26057:FeWrapperSpec class methodsFor: 'registering wrappers'!
{void} registerAbstract: wrapperName {char star}
	with: superName {char star | NULL}
	with: holder {FeWrapperSpecHolder var | NULL}
	
	| wrapper {Sequence} superWrapper {Sequence} |
	wrapper := Sequence string: wrapperName.
	superName == NULL
		ifTrue: [superWrapper := NULL]
		ifFalse: [superWrapper := Sequence string: superName].
	TheWrapperDefs at: wrapper introduce: (FeWrapperDef
		abstract: wrapper with: superWrapper with: holder).!
*/
}
public static void registerDirect(String wrapperName, String superName, FeDirectWrapperMaker maker, FeDirectWrapperChecker checker, FeWrapperSpecHolder holder) {
	Sequence wrapper;
	Sequence superWrapper;
	wrapper = Sequence.string(wrapperName);
	if (superName == null) {
		superWrapper = null;
	}
	else {
		superWrapper = Sequence.string(superName);
	}
	TheWrapperDefs.introduce(wrapper, (FeWrapperDef.makeDirect(wrapper, superWrapper, holder, maker, checker)));
/*
udanax-top.st:26069:FeWrapperSpec class methodsFor: 'registering wrappers'!
{void} registerDirect: wrapperName {char star}
	with: superName {char star | NULL}
	with: maker {FeDirectWrapperMaker var}
	with: checker {FeDirectWrapperChecker var}
	with: holder {FeWrapperSpecHolder var}
	
	| wrapper {Sequence} superWrapper {Sequence} |
	wrapper := Sequence string: wrapperName.
	superName == NULL
		ifTrue: [superWrapper := NULL]
		ifFalse: [superWrapper := Sequence string: superName].
	TheWrapperDefs at: wrapper introduce: (FeWrapperDef
		makeDirect: wrapper
		with: superWrapper
		with: holder
		with: maker
		with: checker).!
*/
}
public static void registerIndirect(String wrapperName, String superName, String innerName, FeIndirectWrapperMaker maker, FeIndirectWrapperChecker checker, FeWrapperSpecHolder holder) {
	Sequence wrapper;
	Sequence superWrapper;
	Sequence innerWrapper;
	wrapper = Sequence.string(wrapperName);
	if (superName == null) {
		superWrapper = null;
	}
	else {
		superWrapper = Sequence.string(superName);
	}
	if (innerName == null) {
		innerWrapper = null;
	}
	else {
		innerWrapper = Sequence.string(innerName);
	}
	TheWrapperDefs.introduce(wrapper, (FeWrapperDef.makeIndirect(wrapper, superWrapper, holder, innerWrapper, maker, checker)));
/*
udanax-top.st:26087:FeWrapperSpec class methodsFor: 'registering wrappers'!
{void} registerIndirect: wrapperName {char star}
	with: superName {char star | NULL}
	with: innerName {char star | NULL}
	with: maker {FeIndirectWrapperMaker var}
	with: checker {FeIndirectWrapperChecker var}
	with: holder {FeWrapperSpecHolder var}
	
	| wrapper {Sequence} superWrapper {Sequence} innerWrapper {Sequence} |
	wrapper := Sequence string: wrapperName.
	superName == NULL
		ifTrue: [superWrapper := NULL]
		ifFalse: [superWrapper := Sequence string: superName].
	innerName == NULL
		ifTrue: [innerWrapper := NULL]
		ifFalse: [innerWrapper := Sequence string: innerName].
	TheWrapperDefs at: wrapper introduce: (FeWrapperDef
		makeIndirect: wrapper
		with: superWrapper
		with: holder
		with: innerWrapper
		with: maker
		with: checker).!
*/
}
/*
udanax-top.st:26112:FeWrapperSpec class methodsFor: 'exceptions: exceptions'!
problems.WrapFailure
	^self signals: #(CannotWrap)!
*/
public static void initTimeNonInherited() {
	TheWrapperDefs = MuTable.make(SequenceSpace.make());
/*
udanax-top.st:26118:FeWrapperSpec class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	
	self REQUIRES: SequenceSpace.
	self REQUIRES: MuTable.
	TheWrapperDefs := MuTable make: SequenceSpace make.!
*/
}
public static void linkTimeNonInherited() {
	TheWrapperDefs = null;
	TheWrapperSpecs = null;
	TheWrapperEndorsements = null;
	TheWrappersFromEndorsements = null;
/*
udanax-top.st:26124:FeWrapperSpec class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	
	TheWrapperDefs := NULL.
	TheWrapperSpecs := NULL.
	TheWrapperEndorsements := NULL.
	TheWrappersFromEndorsements := NULL.!
*/
}
public static void mustSetup() {
	if (TheWrapperEndorsements == null) {
		setWrapperEndorsements(((BeGrandMap) CurrentGrandMap.fluidGet()).wrapperEndorsements());
	}
/*
udanax-top.st:26133:FeWrapperSpec class methodsFor: 'private:'!
{void} mustSetup
	[BeGrandMap] USES.
	TheWrapperEndorsements == NULL ifTrue:
		[self setWrapperEndorsements: CurrentGrandMap fluidGet wrapperEndorsements].!
*/
}
/**
 * Get the local Wrapper spec with the given identifier, or NULL if there is none
 */
public static FeWrapperSpec fetch(Sequence identifier) {
	mustSetup();
	return (FeWrapperSpec) (TheWrapperSpecs.fetch(identifier));
/*
udanax-top.st:26140:FeWrapperSpec class methodsFor: 'accessing'!
{FeWrapperSpec | NULL} fetch: identifier {Sequence}
	"Get the local Wrapper spec with the given identifier, or NULL if there is none"
	
	self mustSetup.
	^(TheWrapperSpecs fetch: identifier) cast: FeWrapperSpec!
*/
}
/**
 * Get the local Wrapper spec with the given identifier, or blast if there is none
 */
public static FeWrapperSpec get(Sequence identifier) {
	FeWrapperSpec result;
	result = fetch(identifier);
	if (result == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return result;
/*
udanax-top.st:26146:FeWrapperSpec class methodsFor: 'accessing'!
{FeWrapperSpec CLIENT} get: identifier {Sequence}
	"Get the local Wrapper spec with the given identifier, or blast if there is none"
	
	| result {FeWrapperSpec} |
	result := self fetch: identifier.
	result == NULL ifTrue:
		[Heaper BLAST: #NotInTable].
	^result!
*/
}
/**
 * Get the endorsements for the named wrapper space
 */
public static CrossRegion getEndorsements(Sequence identifier) {
	mustSetup();
	return (CrossRegion) (TheWrapperEndorsements.get(identifier));
/*
udanax-top.st:26155:FeWrapperSpec class methodsFor: 'accessing'!
{CrossRegion} getEndorsements: identifier {Sequence}
	"Get the endorsements for the named wrapper space"
	
	self mustSetup.
	^(TheWrapperEndorsements get: identifier) cast: CrossRegion!
*/
}
/**
 * Get the wrapper spec corresponding to the given endorsement
 */
public static FeWrapperSpec getFromEndorsement(Tuple endorsement) {
	mustSetup();
	return (FeWrapperSpec) (TheWrappersFromEndorsements.get(endorsement));
/*
udanax-top.st:26161:FeWrapperSpec class methodsFor: 'accessing'!
{FeWrapperSpec} getFromEndorsement: endorsement {Tuple}
	"Get the wrapper spec corresponding to the given endorsement"
	
	self mustSetup.
	^(TheWrappersFromEndorsements get: endorsement) cast: FeWrapperSpec!
*/
}
/**
 * The names of all of the known wrappers
 */
public static XnRegion knownWrappers() {
	return TheWrapperDefs.domain();
/*
udanax-top.st:26167:FeWrapperSpec class methodsFor: 'accessing'!
{XnRegion of: Sequence} knownWrappers
	"The names of all of the known wrappers"
	
	^TheWrapperDefs domain!
*/
}
/**
 * Get the local Wrapper spec with the given identifier, or NULL if there is none
 */
public static void setupWrapperSpecs() {
	TheWrapperSpecs = MuTable.make(SequenceSpace.make());
	Stepper stomper = TheWrapperDefs.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		FeWrapperDef def = (FeWrapperDef) stomper.fetch();
		if (def == null) {
			continue ;
		}
		TheWrapperSpecs.introduce(def.name(), def.makeSpec());
	}
	stomper.destroy();
	Stepper stomper2 = TheWrapperSpecs.stepper();
	for (; stomper2.hasValue(); stomper2.step()) {
		FeWrapperSpec spec = (FeWrapperSpec) stomper2.fetch();
		if (spec == null) {
			continue ;
		}
		spec.setup();
	}
	stomper2.destroy();
/*
udanax-top.st:26172:FeWrapperSpec class methodsFor: 'accessing'!
{void} setupWrapperSpecs
	"Get the local Wrapper spec with the given identifier, or NULL if there is none"
	
	TheWrapperSpecs := MuTable make: SequenceSpace make.
	TheWrapperDefs stepper forEach: [ :def {FeWrapperDef} |
		TheWrapperSpecs at: def name introduce: def makeSpec].
	TheWrapperSpecs stepper forEach: [ :spec {FeWrapperSpec} |
		spec setup].!
*/
}
/**
 * A table mapping from wrapper names to endorsements
 */
public static void setWrapperEndorsements(ScruTable endorsements) {
	TheWrapperEndorsements = endorsements.asMuTable();
	setupWrapperSpecs();
	TheWrappersFromEndorsements = MuTable.make(((BeGrandMap) CurrentGrandMap.fluidGet()).endorsementSpace());
	TableStepper stomper = endorsements.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		Sequence seq = (Sequence) stomper.position();
		CrossRegion endorses = (CrossRegion) stomper.fetch();
		if (endorses == null) {
			continue ;
		}
		if ( ! (endorses.isFinite())) {
			throw new AboraRuntimeException(AboraRuntimeException.FATAL_ERROR);
		}
		Ravi.thingToDo();
		/* implement stepper so that endorsements are allowed to be regions */
		TheWrappersFromEndorsements.introduce(endorses.theOne(), (get(seq))
		/* endorses stepper forEach: [ :endorse {Tuple} |
			TheWrappersFromEndorsements at: endorse
				introduce: (self get: seq)] */
		);
	}
	stomper.destroy();
/*
udanax-top.st:26181:FeWrapperSpec class methodsFor: 'accessing'!
{void} setWrapperEndorsements: endorsements {ScruTable of: Sequence with: CrossRegion}
	"A table mapping from wrapper names to endorsements"
	
	TheWrapperEndorsements := endorsements asMuTable.
	self setupWrapperSpecs.
	TheWrappersFromEndorsements := MuTable make: CurrentGrandMap fluidGet endorsementSpace.
	endorsements stepper forPositions: [ :seq {Sequence} :endorses {CrossRegion} |
		endorses isFinite ifFalse: [Heaper BLAST: #FatalError].
		Ravi thingToDo. "implement stepper so that endorsements are allowed to be regions"
		TheWrappersFromEndorsements at: endorses theOne
			introduce: (self get: seq)
		"endorses stepper forEach: [ :endorse {Tuple} |
			TheWrappersFromEndorsements at: endorse
				introduce: (self get: seq)]"].!
*/
}
/**
 * {Filter CLIENT} filter
 * {Sequence CLIENT} name
 * {FeWrapper CLIENT} wrap: edition {FeEdition}
 */
public static void infostProtocol() {
/*
udanax-top.st:26198:FeWrapperSpec class methodsFor: 'smalltalk: system'!
info.stProtocol
"{Filter CLIENT} filter
{Sequence CLIENT} name
{FeWrapper CLIENT} wrap: edition {FeEdition}
"!
*/
}
public FeWrapperSpec() {
/*

Generated during transformation
*/
}
public FeWrapperSpec(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
