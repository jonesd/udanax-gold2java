/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.be.basic.BeClub;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.filter.Filter;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeKeyMaster;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.primtab.PrimSet;
import info.dgjones.abora.gold.spaces.basic.XnRegion;
import info.dgjones.abora.gold.spaces.unordered.IDSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

/**
 * A KeyMaster provides the authority, or "holds the keys", for a client`s activities on the
 * BackEnd. A client can have any number of different KeyMasters, each with different
 * authority. FeServer_login (if successful) gives you back a KeyMaster with the authority of
 * a single Club (along with all the Clubs of which it is a member, directly or indirectly).
 * This will give you appropriate authority to do anything permitted to that Club. You can
 * incorporate the authority of other KeyMasters into it, so that it will additionally enable
 * you to do anything the other KeyMasters would have enabled.
 */
public class FeKeyMaster extends Heaper {

	protected IDRegion myLoginAuthority;
	protected IDRegion myActualAuthority;
	protected PrimSet myRegisteredWorks;
/*
udanax-top.st:20036:
Heaper subclass: #FeKeyMaster
	instanceVariableNames: '
		myLoginAuthority {IDRegion}
		myActualAuthority {IDRegion}
		myRegisteredWorks {PrimSet | NULL of: FeWork}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:20043:
FeKeyMaster comment:
'A KeyMaster provides the authority, or "holds the keys", for a client`s activities on the BackEnd. A client can have any number of different KeyMasters, each with different authority. FeServer_login (if successful) gives you back a KeyMaster with the authority of a single Club (along with all the Clubs of which it is a member, directly or indirectly). This will give you appropriate authority to do anything permitted to that Club. You can incorporate the authority of other KeyMasters into it, so that it will additionally enable you to do anything the other KeyMasters would have enabled.'!
*/
/*
udanax-top.st:20045:
(FeKeyMaster getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #EQ; yourself)!
*/
/*
udanax-top.st:20178:
FeKeyMaster class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:20181:
(FeKeyMaster getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeKeyMaster.class).setAttributes( new Set().add("CONCRETE").add("ONCLIENT").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential.  The Clubs whose authority is actually being held right now. This may change
 * asynchronously when you or others change the membership lists of clubs.  It is my
 * loginAuthority plus all clubs that list any of these clubs as members, transitively.
 */
public IDRegion actualAuthority() {
	return myActualAuthority;
/*
udanax-top.st:20050:FeKeyMaster methodsFor: 'authority'!
{IDRegion CLIENT} actualAuthority
	"Essential.  The Clubs whose authority is actually being held right now. This may change asynchronously when you or others change the membership lists of clubs.  It is my loginAuthority plus all clubs that list any of these clubs as members, transitively."
	
	^myActualAuthority!
*/
}
/**
 * Essential.  A different KeyMaster with the same login and actual authority as this one.
 */
public FeKeyMaster copy() {
	return FeKeyMaster.make(myLoginAuthority, myActualAuthority);
/*
udanax-top.st:20055:FeKeyMaster methodsFor: 'authority'!
{FeKeyMaster CLIENT} copy
	"Essential.  A different KeyMaster with the same login and actual authority as this one."
	
	^FeKeyMaster make: myLoginAuthority
		with: myActualAuthority!
*/
}
/**
 * Whether this KeyMaster is currently holding the authority of the given Club. Equivalent to
 * this->actualAuthority ()->hasMember (clubID)
 */
public boolean hasAuthority(ID clubID) {
	return myActualAuthority.hasMember(clubID);
/*
udanax-top.st:20061:FeKeyMaster methodsFor: 'authority'!
{BooleanVar CLIENT} hasAuthority: clubID {ID}
	"Whether this KeyMaster is currently holding the authority of the given Club. Equivalent to
		this->actualAuthority ()->hasMember (clubID)"
		
	^myActualAuthority hasMember: clubID!
*/
}
/**
 * Essential.  Add the other KeyMaster's login and actual authorities to my own respective
 * authorities.
 */
public void incorporate(FeKeyMaster other) {
	XnRegion newLogins;
	newLogins = other.loginAuthority().minus(myLoginAuthority);
	myLoginAuthority = (IDRegion) (myLoginAuthority.unionWith(other.loginAuthority()));
	myActualAuthority = (IDRegion) (myActualAuthority.unionWith(other.actualAuthority()));
	/* Tell all my Works */
	authorityChanged();
	Stepper stomper = 
	/* Register with the new login Clubs to find out when their super clubs change */
	newLogins.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		ID login = (ID) stomper.fetch();
		if (login == null) {
			continue ;
		}
		((BeClub) (((BeGrandMap) CurrentGrandMap.fluidGet()).get(login))).registerKeyMaster(this);
	}
	stomper.destroy();
/*
udanax-top.st:20067:FeKeyMaster methodsFor: 'authority'!
{void CLIENT} incorporate: other {FeKeyMaster}
	"Essential.  Add the other KeyMaster's login and actual authorities to my own respective authorities."
	
	| newLogins {XnRegion} |
	newLogins := other loginAuthority minus: myLoginAuthority.
	myLoginAuthority := (myLoginAuthority unionWith: other loginAuthority) cast: IDRegion.
	myActualAuthority := (myActualAuthority unionWith: other actualAuthority) cast: IDRegion.
	"Tell all my Works"
	self authorityChanged.
	"Register with the new login Clubs to find out when their super clubs change"
	newLogins stepper forEach: [ :login {ID} |
		((CurrentGrandMap fluidGet get: login) cast: BeClub) registerKeyMaster: self]!
*/
}
/**
 * Essential.  The Clubs whose authority was obtained directly, by logging in to them. They
 * are the ones from which all other authority is derived.
 */
public IDRegion loginAuthority() {
	return myLoginAuthority;
/*
udanax-top.st:20080:FeKeyMaster methodsFor: 'authority'!
{IDRegion CLIENT} loginAuthority
	"Essential.  The Clubs whose authority was obtained directly, by logging in to them. They are the ones from which all other authority is derived."
	
	^myLoginAuthority!
*/
}
/**
 * Essential.  Remove the listed IDs from the set of Clubs whose login authority I exercise.
 * All authority derived from them that cannot be derived from the remaining login authority
 * will also disappear.  Listed Clubs for which I do not hold login authority will be
 * silently ignored.
 */
public void removeLogins(IDRegion oldLogins) {
	IDRegion removed;
	removed = (IDRegion) (oldLogins.intersect(myLoginAuthority));
	myLoginAuthority = (IDRegion) (myLoginAuthority.minus(removed));
	/* Figure out the new transitive authority */
	updateAuthority();
	Stepper stomper = 
	/* Unregister with the new IDs */
	removed.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		ID login = (ID) stomper.fetch();
		if (login == null) {
			continue ;
		}
		((BeClub) (((BeGrandMap) CurrentGrandMap.fluidGet()).get(login))).unregisterKeyMaster(this);
	}
	stomper.destroy();
/*
udanax-top.st:20085:FeKeyMaster methodsFor: 'authority'!
{void CLIENT} removeLogins: oldLogins {IDRegion}
	"Essential.  Remove the listed IDs from the set of Clubs whose login authority I exercise.  All authority derived from them that cannot be derived from the remaining login authority will also disappear.  Listed Clubs for which I do not hold login authority will be silently ignored."
	
	| removed {IDRegion} |
	removed := (oldLogins intersect: myLoginAuthority) cast: IDRegion.
	myLoginAuthority := (myLoginAuthority minus: removed) cast: IDRegion.
	"Figure out the new transitive authority"
	self updateAuthority.
	"Unregister with the new IDs"
	removed stepper forEach: [ :login {ID} |
		((CurrentGrandMap fluidGet get: login) cast: BeClub) unregisterKeyMaster: self]!
*/
}
public FeKeyMaster(IDRegion loginAuthority, IDRegion actualAuthority) {
	super();
	myLoginAuthority = loginAuthority;
	myActualAuthority = actualAuthority;
	myRegisteredWorks = null;
/*
udanax-top.st:20099:FeKeyMaster methodsFor: 'private: create'!
create: loginAuthority {IDRegion}
	with: actualAuthority {IDRegion}
	super create.
	myLoginAuthority := loginAuthority.
	myActualAuthority := actualAuthority.
	myRegisteredWorks := NULL.!
*/
}
/**
 * Whether this KeyMaster has signature authority for the given Club
 */
public boolean hasSignatureAuthority(ID club) {
	ID sig;
	BeGrandMap cgm;
	cgm = ((BeGrandMap) CurrentGrandMap.fluidGet());
	return (sig = (cgm.getClub(club)).fetchSignatureClub()) != null && (hasAuthority(sig));
/*
udanax-top.st:20109:FeKeyMaster methodsFor: 'server accessing'!
{BooleanVar} hasSignatureAuthority: club {ID}
	"Whether this KeyMaster has signature authority for the given Club"
	
	| sig {ID} cgm {BeGrandMap} |
	cgm := CurrentGrandMap fluidGet.
	^(sig := (cgm getClub: club) fetchSignatureClub) ~~ NULL
		and: [self hasAuthority: sig]!
*/
}
/**
 * Notify the Work whenever my authority changes
 */
public void registerWork(FeWork work) {
	if (myRegisteredWorks == null) {
		myRegisteredWorks = PrimSet.weak();
	}
	myRegisteredWorks.introduce(work);
/*
udanax-top.st:20117:FeKeyMaster methodsFor: 'server accessing'!
{void} registerWork: work {FeWork}
	"Notify the Work whenever my authority changes"
	
	myRegisteredWorks == NULL ifTrue:
		[myRegisteredWorks := PrimSet weak].
	myRegisteredWorks introduce: work!
*/
}
/**
 * Notify the Work whenever my authority changes
 */
public void unregisterWork(FeWork work) {
	if (myRegisteredWorks == null || (myRegisteredWorks.isEmpty())) {
		throw new AboraRuntimeException(AboraRuntimeException.NEVER_ADDED_WATCHER);
	}
	myRegisteredWorks.remove(work);
	if (myRegisteredWorks.isEmpty()) {
		myRegisteredWorks = null;
	}
/*
udanax-top.st:20124:FeKeyMaster methodsFor: 'server accessing'!
{void} unregisterWork: work {FeWork}
	"Notify the Work whenever my authority changes"
	
	(myRegisteredWorks == NULL or: [myRegisteredWorks isEmpty]) ifTrue:
		[Heaper BLAST: #NeverAddedWatcher]. 
	myRegisteredWorks remove: work.
	myRegisteredWorks isEmpty ifTrue:
		[myRegisteredWorks := NULL].!
*/
}
/**
 * Recompute the actual authority of this KeyMaster based on the set of login Clubs
 */
public void updateAuthority() {
	myActualAuthority = ((IDRegion) IDSpace.global().emptyRegion());
	Stepper stomper = myLoginAuthority.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		ID login = (ID) stomper.fetch();
		if (login == null) {
			continue ;
		}
		myActualAuthority = (IDRegion) (myActualAuthority.unionWith(((BeClub) (((BeGrandMap) CurrentGrandMap.fluidGet()).get(login))).transitiveSuperClubIDs()));
	}
	stomper.destroy();
	authorityChanged();
/*
udanax-top.st:20133:FeKeyMaster methodsFor: 'server accessing'!
{void} updateAuthority
	"Recompute the actual authority of this KeyMaster based on the set of login Clubs"
	
	myActualAuthority := (IDSpace global emptyRegion cast: IDRegion).
	myLoginAuthority stepper forEach: [ :login {ID} |
		myActualAuthority := (myActualAuthority
			unionWith: ((CurrentGrandMap fluidGet get: login) cast: BeClub) transitiveSuperClubIDs) cast: IDRegion].
	self authorityChanged.!
*/
}
/**
 * Notify all my dependents of a change in authority
 */
public void authorityChanged() {
	if (myRegisteredWorks != null) {
		Stepper stomper = myRegisteredWorks.stepper();
		for (; stomper.hasValue(); stomper.step()) {
			FeWork work = (FeWork) stomper.fetch();
			if (work == null) {
				continue ;
			}
			work.updateStatus();
		}
		stomper.destroy();
	}
/*
udanax-top.st:20144:FeKeyMaster methodsFor: 'private:'!
{void} authorityChanged
	"Notify all my dependents of a change in authority"
	
	myRegisteredWorks ~~ NULL ifTrue:
		[myRegisteredWorks stepper forEach: [ :work {FeWork} |
			work updateStatus]]!
*/
}
public void printOn(PrintWriter oo) {
	oo.print("KeyMaster(");
	oo.print(loginAuthority());
	oo.print(")");
/*
udanax-top.st:20153:FeKeyMaster methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << 'KeyMaster(' << self loginAuthority << ')'!
*/
}
/**
 * A filter for things which can be read by this KeyMaster
 */
public Filter permissionsFilter() {
	Someone.thingToDo();
	/* have all callers use 'actualAuthority' instead */
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).globalIDFilterSpace().anyFilter(myActualAuthority);
/*
udanax-top.st:20159:FeKeyMaster methodsFor: 'obsolete:'!
{Filter} permissionsFilter
	"A filter for things which can be read by this KeyMaster"
	
	self thingToDo. "have all callers use 'actualAuthority' instead"
	^CurrentGrandMap fluidGet globalIDFilterSpace anyFilter: myActualAuthority!
*/
}
/**
 * @deprecated
 */
public void removeAuthority(IDRegion oldLogins) {
	throw new PasseException();
/*
udanax-top.st:20167:FeKeyMaster methodsFor: 'smalltalk: passe'!
{void} removeAuthority: oldLogins {IDRegion}
	self passe.	"renamed removeLogins:"!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:20173:FeKeyMaster methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:20175:FeKeyMaster methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
/**
 * Make a KeyMaster initially logged in to the given Club
 */
public static FeKeyMaster make(ID clubID) {
	return make(((IDRegion) clubID.asRegion())
	/* login authority */
	, (((BeGrandMap) CurrentGrandMap.fluidGet()).getClub(clubID)).transitiveSuperClubIDs());
/*
udanax-top.st:20186:FeKeyMaster class methodsFor: 'creation'!
make: clubID {ID}
	"Make a KeyMaster initially logged in to the given Club"
	
	^self make: (clubID asRegion cast: IDRegion) "login authority"
		with: (CurrentGrandMap fluidGet getClub: clubID) transitiveSuperClubIDs!
*/
}
/**
 * Make a KeyMaster initially logged in to the given Clubs
 */
public static FeKeyMaster makeAll(IDRegion clubIDs) {
	IDRegion actuals;
	BeGrandMap gm;
	gm = ((BeGrandMap) CurrentGrandMap.fluidGet());
	actuals = (IDRegion) gm.globalIDSpace().emptyRegion();
	Stepper stomper = clubIDs.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		ID iD = (ID) stomper.fetch();
		if (iD == null) {
			continue ;
		}
		actuals = (IDRegion) (actuals.unionWith((gm.getClub(iD)).transitiveSuperClubIDs()));
	}
	stomper.destroy();
	return make(clubIDs, actuals);
/*
udanax-top.st:20192:FeKeyMaster class methodsFor: 'creation'!
{FeKeyMaster} makeAll: clubIDs {IDRegion}
	"Make a KeyMaster initially logged in to the given Clubs"
	
	| actuals {IDRegion} gm {BeGrandMap} |
	gm := CurrentGrandMap fluidGet.
	actuals := gm globalIDSpace emptyRegion cast: IDRegion.
	clubIDs stepper forEach: [:iD {ID} |
		actuals := (actuals unionWith: (gm getClub: iD) transitiveSuperClubIDs) cast: IDRegion].
	^self make: clubIDs with: actuals!
*/
}
/**
 * Make a KeyMaster logged in to the Universal Public Club.
 */
public static FeKeyMaster makePublic() {
	return FeKeyMaster.make(FeServer.publicClubID());
/*
udanax-top.st:20202:FeKeyMaster class methodsFor: 'creation'!
{FeKeyMaster} makePublic
	"Make a KeyMaster logged in to the Universal Public Club."
	
	^FeKeyMaster make: FeServer publicClubID!
*/
}
public static FeKeyMaster make(IDRegion loginAuthority, IDRegion actualAuthority) {
	FeKeyMaster result;
	result = new FeKeyMaster(loginAuthority, actualAuthority);
	Stepper stomper = 
	/* Register with all the login Clubs to find out when their permissions change */
	loginAuthority.stepper();
	for (; stomper.hasValue(); stomper.step()) {
		ID loginClubID = (ID) stomper.fetch();
		if (loginClubID == null) {
			continue ;
		}
		((BeClub) (((BeGrandMap) CurrentGrandMap.fluidGet()).get(loginClubID))).registerKeyMaster(result);
	}
	stomper.destroy();
	return result;
/*
udanax-top.st:20209:FeKeyMaster class methodsFor: 'private: pseudo constructors'!
make: loginAuthority {IDRegion}
	with: actualAuthority {IDRegion}
	
	| result {FeKeyMaster} |
	result := self create: loginAuthority with: actualAuthority.
	"Register with all the login Clubs to find out when their permissions change"
	loginAuthority stepper forEach: [ :loginClubID {ID} |
		((CurrentGrandMap fluidGet get: loginClubID) cast: BeClub)
			registerKeyMaster: result].
	^result!
*/
}
/**
 * {IDRegion CLIENT} actualAuthority
 * {FeKeyMaster CLIENT} copy
 * {BooleanVar CLIENT} hasAuthority: clubID {ID}
 * {void CLIENT} incorporate: other {FeKeyMaster}
 * {IDRegion CLIENT} loginAuthority
 * {void CLIENT} removeLogins: oldLogins {IDRegion}
 */
public static void infostProtocol() {
/*
udanax-top.st:20222:FeKeyMaster class methodsFor: 'smalltalk: system'!
info.stProtocol
"{IDRegion CLIENT} actualAuthority
{FeKeyMaster CLIENT} copy
{BooleanVar CLIENT} hasAuthority: clubID {ID}
{void CLIENT} incorporate: other {FeKeyMaster}
{IDRegion CLIENT} loginAuthority
{void CLIENT} removeLogins: oldLogins {IDRegion}
"!
*/
}
/**
 * Blast if the CurrentKeyMaster doesn't have Admin authority.
 */
public static void assertAdminAuthority() {
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasAuthority(((BeGrandMap) CurrentGrandMap.fluidGet()).adminClubID()))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_ADMIN_AUTHORITY);
	}
/*
udanax-top.st:20233:FeKeyMaster class methodsFor: 'assertions'!
{void} assertAdminAuthority
	"Blast if the CurrentKeyMaster doesn't have Admin authority."
	(CurrentKeyMaster fluidGet hasAuthority: CurrentGrandMap fluidGet adminClubID)
		ifFalse: [Heaper BLAST: #MustHaveAdminAuthority].!
*/
}
/**
 * Blast if the CurrentKeyMaster doesn't have signature authority for the CurrentAuthor.
 */
public static void assertSignatureAuthority() {
	if ( ! (((FeKeyMaster) CurrentKeyMaster.fluidGet()).hasSignatureAuthority(((ID) CurrentAuthor.fluidGet())))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_AUTHOR_SIGNATURE_AUTHORITY);
	}
/*
udanax-top.st:20239:FeKeyMaster class methodsFor: 'assertions'!
{void} assertSignatureAuthority
	"Blast if the CurrentKeyMaster doesn't have signature authority for the CurrentAuthor."
	(CurrentKeyMaster fluidGet hasSignatureAuthority: CurrentAuthor fluidGet) ifFalse:
		[Heaper BLAST: #MustHaveAuthorSignatureAuthority].!
*/
}
/**
 * If there is a currentSponsor, then the CurrentKeyMaster must have authority for it.
 */
public static void assertSponsorship() {
	FeKeyMaster ckm;
	BeGrandMap cgm;
	ckm = ((FeKeyMaster) CurrentKeyMaster.fluidGet());
	cgm = ((BeGrandMap) CurrentGrandMap.fluidGet());
	if ( ! (((ID) InitialSponsor.fluidGet()) == cgm.emptyClubID() || (ckm.hasAuthority(((ID) InitialSponsor.fluidFetch()))))) {
		throw new AboraRuntimeException(AboraRuntimeException.MUST_HAVE_SPONSOR_AUTHORITY);
	}
/*
udanax-top.st:20244:FeKeyMaster class methodsFor: 'assertions'!
{void} assertSponsorship
	"If there is a currentSponsor, then the CurrentKeyMaster must have authority for it."
	| ckm {FeKeyMaster} cgm {BeGrandMap} |
	ckm := CurrentKeyMaster fluidGet.
	cgm := CurrentGrandMap fluidGet.
	(InitialSponsor fluidGet == cgm emptyClubID
		or: [ckm hasAuthority: InitialSponsor fluidFetch])
		ifFalse: [Heaper BLAST: #MustHaveSponsorAuthority]!
*/
}
public FeKeyMaster() {
/*

Generated during transformation
*/
}
public FeKeyMaster(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
