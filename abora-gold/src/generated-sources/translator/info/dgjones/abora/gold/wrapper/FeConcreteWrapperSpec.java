/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.wrapper;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.wrapper.FeConcreteWrapperSpec;
import info.dgjones.abora.gold.wrapper.FeWrapper;
import info.dgjones.abora.gold.wrapper.FeWrapperDef;
import info.dgjones.abora.gold.wrapper.FeWrapperSpec;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class FeConcreteWrapperSpec extends FeWrapperSpec {

/*
udanax-top.st:26291:
FeWrapperSpec subclass: #FeConcreteWrapperSpec
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-wrapper'!
*/
/*
udanax-top.st:26295:
(FeConcreteWrapperSpec getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeConcreteWrapperSpec.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void setup() {
	super.setup();
	if (fetchSuperSpec() != null) {
		fetchSuperSpec().setupConcreteSubSpec(this);
	}
/*
udanax-top.st:26300:FeConcreteWrapperSpec methodsFor: 'protected:'!
{void} setup
	super setup.
	self fetchSuperSpec ~~ NULL ifTrue:
		[self fetchSuperSpec setupConcreteSubSpec: self].!
*/
}
public boolean certify(FeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:26308:FeConcreteWrapperSpec methodsFor: 'accessing'!
{BooleanVar} certify: edition {FeEdition}
	self subclassResponsibility!
*/
}
/**
 * Whether I can wrap the given type
 */
public boolean wraps(FeConcreteWrapperSpec other) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:26312:FeConcreteWrapperSpec methodsFor: 'accessing'!
{BooleanVar} wraps: other {FeConcreteWrapperSpec}
	"Whether I can wrap the given type"
	self subclassResponsibility!
*/
}
public FeConcreteWrapperSpec(FeWrapperDef def) {
	super(def);
/*
udanax-top.st:26318:FeConcreteWrapperSpec methodsFor: 'create'!
create: def {FeWrapperDef}
	super create: def!
*/
}
/**
 * Endorse an Edition as being of this type
 */
public void endorse(FeEdition edition) {
	edition.beEdition().endorse(endorsements());
/*
udanax-top.st:26323:FeConcreteWrapperSpec methodsFor: 'for wrappers only'!
{void} endorse: edition {FeEdition}
	"Endorse an Edition as being of this type"
	
	[BeEdition] USES.
	edition beEdition endorse: self endorsements!
*/
}
public FeWrapper fetchWrap(FeEdition edition) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:26331:FeConcreteWrapperSpec methodsFor: 'vulnerable'!
{FeWrapper | NULL} fetchWrap: edition {FeEdition}
	self subclassResponsibility!
*/
}
public FeConcreteWrapperSpec() {
/*

Generated during transformation
*/
}
public FeConcreteWrapperSpec(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
