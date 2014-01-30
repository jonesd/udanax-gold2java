/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nbacken;

import info.dgjones.abora.gold.be.basic.BeEdition;
import info.dgjones.abora.gold.be.basic.BeGrandMap;
import info.dgjones.abora.gold.collection.steppers.Stepper;
import info.dgjones.abora.gold.collection.steppers.TableStepper;
import info.dgjones.abora.gold.id.IDRegion;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nbacken.GrantStepper;
import info.dgjones.abora.gold.nkernel.FeBundle;
import info.dgjones.abora.gold.nkernel.FeElementBundle;
import info.dgjones.abora.gold.spaces.basic.Position;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Has a Bundle Stepper on a piece of the Edition defining the grants for this Server, and
 * views it as a sequence of associations from ClubIDs to IDRegions (which is the inverse of
 * its actual format)
 */
public class GrantStepper extends TableStepper {

	protected Stepper myBundles;
	protected IDRegion myClubIDs;
/*
udanax-top.st:55728:
TableStepper subclass: #GrantStepper
	instanceVariableNames: '
		myBundles {Stepper of: FeBundle}
		myClubIDs {IDRegion | NULL}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nbacken'!
*/
/*
udanax-top.st:55734:
GrantStepper comment:
'Has a Bundle Stepper on a piece of the Edition defining the grants for this Server, and views it as a sequence of associations from ClubIDs to IDRegions (which is the inverse of its actual format)'!
*/
/*
udanax-top.st:55736:
(GrantStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:55778:
GrantStepper class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:55781:
(GrantStepper getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrantStepper.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public Position position() {
	Ravi.thingToDo();
	/* in future implementations this might also get back an ArrayBundle */
	return ((BeGrandMap) CurrentGrandMap.fluidGet()).iDOf(((FeElementBundle) myBundles.get()).element().getOrMakeBe());
/*
udanax-top.st:55741:GrantStepper methodsFor: 'special'!
{Position} position
	Ravi thingToDo. "in future implementations this might also get back an ArrayBundle"
	^CurrentGrandMap fluidGet iDOf: (myBundles get cast: FeElementBundle) element getOrMakeBe!
*/
}
public Heaper fetch() {
	FeBundle bundle;
	bundle = (FeBundle) myBundles.fetch();
	if (bundle == null) {
		return null;
	}
	return bundle.region();
/*
udanax-top.st:55748:GrantStepper methodsFor: 'operations'!
{Heaper wimpy} fetch
	| bundle {FeBundle} |
	bundle := myBundles fetch cast: FeBundle.
	bundle == NULL ifTrue: [^NULL].
	^bundle region!
*/
}
public boolean hasValue() {
	return myBundles.hasValue();
/*
udanax-top.st:55755:GrantStepper methodsFor: 'operations'!
{BooleanVar} hasValue
	^myBundles hasValue!
*/
}
public void step() {
	while (myBundles.hasValue()) {
		myBundles.step();
		if (myClubIDs == null || (myClubIDs.hasMember(position()))) {
			return ;
		}
	}
/*
udanax-top.st:55759:GrantStepper methodsFor: 'operations'!
{void} step
	[myBundles hasValue] whileTrue:
		[myBundles step.
		(myClubIDs == NULL or: [myClubIDs hasMember: self position])
			ifTrue: [^VOID]]!
*/
}
public Stepper copy() {
	return new GrantStepper(myBundles.copy(), myClubIDs);
/*
udanax-top.st:55767:GrantStepper methodsFor: 'create'!
{Stepper} copy
	^GrantStepper create: myBundles copy with: myClubIDs!
*/
}
public GrantStepper(Stepper bundles, IDRegion clubIDs) {
	super();
	myBundles = bundles;
	myClubIDs = clubIDs;
/*
udanax-top.st:55771:GrantStepper methodsFor: 'create'!
create: bundles {Stepper of: FeBundle} with: clubIDs {IDRegion | NULL}
	super create.
	myBundles := bundles.
	myClubIDs := clubIDs.!
*/
}
public static TableStepper make(BeEdition grants, IDRegion clubIDs) {
	return new GrantStepper(grants.retrieve(), clubIDs);
/*
udanax-top.st:55786:GrantStepper class methodsFor: 'create'!
{TableStepper} make: grants {BeEdition} with: clubIDs {IDRegion | NULL}
	^self create: grants retrieve with: clubIDs!
*/
}
public GrantStepper() {
/*

Generated during transformation
*/
}
public GrantStepper(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
