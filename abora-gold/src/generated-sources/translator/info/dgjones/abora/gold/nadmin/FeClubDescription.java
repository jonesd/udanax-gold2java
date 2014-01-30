/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nadmin;

import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nadmin.FeClubDescription;
import info.dgjones.abora.gold.nadmin.FeLockSmith;
import info.dgjones.abora.gold.nadmin.FeWallLockSmith;
import info.dgjones.abora.gold.nkernel.FeClub;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.tumbler.Sequence;
import info.dgjones.abora.gold.tumbler.SequenceSpace;
import info.dgjones.abora.gold.wrapper.FeSet;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;
import java.io.PrintWriter;

/**
 * Describes the state of Club -- who is in it, which Work is its home (if it has one), and
 * how you can login to it
 */
public class FeClubDescription extends FeWrapper {

	protected static FeWrapperSpec TheClubDescriptionSpec;
/*
udanax-top.st:23696:
FeWrapper subclass: #FeClubDescription
	instanceVariableNames: ''
	classVariableNames: 'TheClubDescriptionSpec {FeWrapperSpec} '
	poolDictionaries: ''
	category: 'Xanadu-nadmin'!
*/
/*
udanax-top.st:23700:
FeClubDescription comment:
'Describes the state of Club -- who is in it, which Work is its home (if it has one), and how you can login to it'!
*/
/*
udanax-top.st:23702:
(FeClubDescription getOrMakeCxxClassDescription)
	friends:
'/- friends for class FeClubDescription -/
friend class BeClub;
';
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:23775:
FeClubDescription class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:23778:
(FeClubDescription getOrMakeCxxClassDescription)
	friends:
'/- friends for class FeClubDescription -/
friend class BeClub;
';
	attributes: ((Set new) add: #ON.CLIENT; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeClubDescription.class).setAttributes( new Set().add("ONCLIENT").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Describes how authority for this Club is gained
 */
public FeLockSmith lockSmith() {
	if (edition().includesKey((Sequence.string("ClubDescription:LockSmith")))) {
		return (FeLockSmith) (FeLockSmith.spec().wrap(((FeEdition) (edition().get((Sequence.string("ClubDescription:LockSmith")))))));
	}
	else {
		return FeWallLockSmith.make();
	}
/*
udanax-top.st:23712:FeClubDescription methodsFor: 'accessing'!
{FeLockSmith CLIENT} lockSmith
	"Describes how authority for this Club is gained"
	
	(self edition includesKey: (Sequence string: 'ClubDescription:LockSmith')) ifTrue:
		[^(FeLockSmith spec wrap: ((self edition
			get: (Sequence string: 'ClubDescription:LockSmith'))
				cast: FeEdition)) cast: FeLockSmith]
	ifFalse: [^FeWallLockSmith make]!
*/
}
/**
 * The Clubs which are members of this one.
 */
public FeSet membership() {
	if (edition().includesKey((Sequence.string("ClubDescription:Membership")))) {
		return (FeSet) (FeSet.spec().wrap(((FeEdition) (edition().get((Sequence.string("ClubDescription:Membership")))))));
	}
	else {
		return FeSet.make();
	}
/*
udanax-top.st:23721:FeClubDescription methodsFor: 'accessing'!
{FeSet CLIENT of: FeClub} membership
	"The Clubs which are members of this one."
	
	(self edition includesKey: (Sequence string: 'ClubDescription:Membership')) ifTrue:
		[^(FeSet spec wrap: ((self edition
			get: (Sequence string: 'ClubDescription:Membership'))
				cast: FeEdition)) cast: FeSet]
	ifFalse: [^FeSet make]!
*/
}
/**
 * Change how authority is gained
 */
public FeClubDescription withLockSmith(FeLockSmith lockSmith) {
	return FeClubDescription.construct((edition().with((Sequence.string("ClubDescription:LockSmith")), lockSmith.edition())));
/*
udanax-top.st:23730:FeClubDescription methodsFor: 'accessing'!
{FeClubDescription CLIENT} withLockSmith: lockSmith {FeLockSmith}
	"Change how authority is gained"
	^FeClubDescription construct: (self edition with: (Sequence string: 'ClubDescription:LockSmith')
		with: lockSmith edition)!
*/
}
/**
 * Change the entire membership list
 */
public FeClubDescription withMembership(FeSet members) {
	return FeClubDescription.construct((edition().with((Sequence.string("ClubDescription:Membership")), members.edition())));
/*
udanax-top.st:23736:FeClubDescription methodsFor: 'accessing'!
{FeClubDescription CLIENT} withMembership: members {FeSet of: FeClub}
	"Change the entire membership list"
	^FeClubDescription construct: (self edition with: (Sequence string: 'ClubDescription:Membership')
		with: members edition)!
*/
}
public FeClubDescription(FeEdition edition, FeWrapperSpec spec) {
	super(edition, spec);
/*
udanax-top.st:23744:FeClubDescription methodsFor: 'private: create'!
create: edition {FeEdition} with: spec {FeWrapperSpec}
	super create: edition with: spec!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(lockSmith());
	oo.print(", ");
	oo.print(membership());
	oo.print(")");
/*
udanax-top.st:23750:FeClubDescription methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '('
		<< self lockSmith << ', '
		<< self membership << ')'!
*/
}
/**
 * The Work which is the home for this Club; blasts if it has none
 * @deprecated
 */
public FeWork home() {
	throw new PasseException();
/*
udanax-top.st:23758:FeClubDescription methodsFor: 'smalltalk: passe'!
{FeWork} home
	"The Work which is the home for this Club; blasts if it has none"
	self passe.
	^(self edition get: (Sequence string: 'ClubDescription:Home')) cast: FeWork!
*/
}
/**
 * Change the home to different Work, or to none if NULL
 * @deprecated
 */
public FeClubDescription withHome(FeWork home) {
	throw new PasseException();
/*
udanax-top.st:23763:FeClubDescription methodsFor: 'smalltalk: passe'!
{FeClubDescription} withHome: home {FeWork | NULL}
	"Change the home to different Work, or to none if NULL"
	self passe.
	home == NULL ifTrue:
		[^FeClubDescription construct: (self edition
			without: (Sequence string: 'ClubDescription:Home'))]
	ifFalse:
		[^FeClubDescription construct: (self edition
			with: (Sequence string: 'ClubDescription:Home')
			with: home edition)]!
*/
}
/**
 * Check that it has the right fields in the right places. Ignore other contents.
 */
public static boolean check(FeEdition edition) {
	if ( ! ((FeWrapper.checkDomainIn(edition, ((Sequence.string("ClubDescription:LockSmith")).asRegion().with((Sequence.string("ClubDescription:Membership")))))) && ((FeWrapper.checkSubEdition(edition, (Sequence.string("ClubDescription:Membership")), FeSet.spec(), false)) && (FeWrapper.checkSubEdition(edition, (Sequence.string("ClubDescription:LockSmith")), FeLockSmith.spec(), false))))) {
		return false;
	}
	if (edition.includesKey((Sequence.string("ClubDescription:Membership")))) {
		FeEdition sub;
		sub = (FeEdition) (edition.get((Sequence.string("ClubDescription:Membership"))));
		Stepper stomper = sub.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			FeRangeElement r = (FeRangeElement) stomper.fetch();
			if (r == null) {
				continue ;
			}
			if ( ! (r instanceof FeClub)) {
				return false;
			}
		}
		stomper.destroy();
	}
	return true;
/*
udanax-top.st:23788:FeClubDescription class methodsFor: 'private: wrapping'!
{BooleanVar} check: edition {FeEdition}
	"Check that it has the right fields in the right places. Ignore other contents."
	
	((FeWrapper checkDomainIn: edition
		with: ((Sequence string: 'ClubDescription:LockSmith') asRegion
			with: (Sequence string: 'ClubDescription:Membership')))
		and: [(FeWrapper checkSubEdition: edition
			with: (Sequence string: 'ClubDescription:Membership')
			with: FeSet spec
			with: false)
		and: [FeWrapper checkSubEdition: edition
			with: (Sequence string: 'ClubDescription:LockSmith')
			with: FeLockSmith spec
			with: false]])
		ifFalse: [^false].
	(edition includesKey: (Sequence string: 'ClubDescription:Membership')) ifTrue:
		[ | sub {FeEdition} |
		sub := (edition get: (Sequence string: 'ClubDescription:Membership')) cast: FeEdition.
		sub stepper forEach: [ :r {FeRangeElement} |
			(r isKindOf: FeClub) ifFalse: [^false]]].
	^true!
*/
}
/**
 * Create a new wrapper and endorse it
 */
public static FeClubDescription construct(FeEdition edition) {
	spec().endorse(edition);
	return (FeClubDescription) (makeWrapper(edition));
/*
udanax-top.st:23810:FeClubDescription class methodsFor: 'private: wrapping'!
{FeClubDescription} construct: edition {FeEdition}
	"Create a new wrapper and endorse it"
	
	self spec endorse: edition.
	^(self makeWrapper: edition) cast: FeClubDescription!
*/
}
/**
 * Just create a new wrapper
 */
public static FeWrapper makeWrapper(FeEdition edition) {
	return new FeClubDescription(edition, spec());
/*
udanax-top.st:23816:FeClubDescription class methodsFor: 'private: wrapping'!
{FeWrapper} makeWrapper: edition {FeEdition}
	"Just create a new wrapper"
	
	^self create: edition with: self spec!
*/
}
public static void setSpec(FeWrapperSpec wrap) {
	TheClubDescriptionSpec = wrap;
/*
udanax-top.st:23821:FeClubDescription class methodsFor: 'private: wrapping'!
{void} setSpec: wrap {FeWrapperSpec}
	TheClubDescriptionSpec := wrap.!
*/
}
public static void initTimeNonInherited() {
	FeWrapperSpec.DIRECTWRAPPER("ClubDescription", "Wrapper", FE_CLUB_DESCRIPTION);
/*
udanax-top.st:23827:FeClubDescription class methodsFor: 'smalltalk: init'!
initTimeNonInherited
	FeWrapperSpec DIRECTWRAPPER: 'ClubDescription' with: 'Wrapper' with: #FeClubDescription.!
*/
}
public static void linkTimeNonInherited() {
	TheClubDescriptionSpec = null;
/*
udanax-top.st:23831:FeClubDescription class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	TheClubDescriptionSpec := NULL.!
*/
}
public static FeClubDescription make(FeSet membership, FeLockSmith lockSmith) {
	FeEdition result;
	result = FeEdition.empty(SequenceSpace.make());
	if (membership != null) {
		result = result.with((Sequence.string("ClubDescription:Membership")), membership.edition());
	}
	if (lockSmith != null) {
		result = result.with((Sequence.string("ClubDescription:LockSmith")), lockSmith.edition());
	}
	return (FeClubDescription) (spec().wrap(result));
/*
udanax-top.st:23837:FeClubDescription class methodsFor: 'pseudo constructors'!
{FeClubDescription CLIENT} make: membership {(FeSet of: FeClub) | NULL}
	with: lockSmith {FeLockSmith default: NULL}
	| result {FeEdition} |
	result := FeEdition empty: SequenceSpace make.
	membership ~~ NULL ifTrue:
		[result := result with: (Sequence string: 'ClubDescription:Membership')
			with: membership edition].
	lockSmith ~~ NULL ifTrue:
		[result := result with: (Sequence string: 'ClubDescription:LockSmith')
			with: lockSmith edition].
	^(self spec wrap: result) cast: FeClubDescription!
*/
}
public static FeWrapperSpec spec() {
	return TheClubDescriptionSpec;
/*
udanax-top.st:23850:FeClubDescription class methodsFor: 'pseudo constructors'!
{FeWrapperSpec} spec
	^TheClubDescriptionSpec!
*/
}
/**
 * @deprecated
 */
public static FeClubDescription make(FeSet members, FeLockSmith lockSmith, FeWork home) {
	throw new PasseException();
/*
udanax-top.st:23856:FeClubDescription class methodsFor: 'smalltalk: passe'!
make: members {FeSet | NULL}
	with: lockSmith {FeLockSmith | NULL}
	with: home {FeWork | NULL}
	| result {FeEdition} |
	self passe.
	result := FeEdition empty: SequenceSpace make.
	members ~~ NULL ifTrue:
		[result := result with: (Sequence string: 'ClubDescription:Membership')
			with: members edition].
	lockSmith ~~ NULL ifTrue:
		[result := result with: (Sequence string: 'ClubDescription:LockSmith')
			with: lockSmith edition].
	home ~~ NULL ifTrue:
		[result := result with: (Sequence string: 'ClubDescription:Home')
			with: home].
	^(self spec wrap: result) cast: FeClubDescription!
*/
}
/**
 * {FeLockSmith CLIENT} lockSmith
 * {FeSet CLIENT of: FeClub} membership
 * {FeClubDescription CLIENT} withLockSmith: lockSmith {FeLockSmith}
 * {FeClubDescription CLIENT} withMembership: members {FeSet of: FeClub}
 */
public static void infostProtocol() {
/*
udanax-top.st:23876:FeClubDescription class methodsFor: 'smalltalk: system'!
info.stProtocol
"{FeLockSmith CLIENT} lockSmith
{FeSet CLIENT of: FeClub} membership
{FeClubDescription CLIENT} withLockSmith: lockSmith {FeLockSmith}
{FeClubDescription CLIENT} withMembership: members {FeSet of: FeClub}
"!
*/
}
public FeClubDescription() {
/*

Generated during transformation
*/
}
public FeClubDescription(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
