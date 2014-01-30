/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.filter;

import info.dgjones.abora.gold.filter.RealDsp;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.basic.CoordinateSpace;
import info.dgjones.abora.gold.spaces.basic.Dsp;
import info.dgjones.abora.gold.spaces.unordered.IdentityDsp;
import info.dgjones.abora.gold.tumbler.RealSpace;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class RealDsp extends IdentityDsp {

/*
udanax-top.st:29730:
IdentityDsp subclass: #RealDsp
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Filter'!
*/
/*
udanax-top.st:29734:
(RealDsp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:29752:
RealDsp class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:29755:
(RealDsp getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RealDsp.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public CoordinateSpace coordinateSpace() {
	return RealSpace.make();
/*
udanax-top.st:29739:RealDsp methodsFor: 'deferred accessing'!
{CoordinateSpace} coordinateSpace
	^RealSpace make!
*/
}
public RealDsp(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:29745:RealDsp methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:29748:RealDsp methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static Dsp make() {
	return new RealDsp();
/*
udanax-top.st:29760:RealDsp class methodsFor: 'creation'!
{Dsp} make
	
	^self create!
*/
}
public RealDsp() {
/*

Generated during transformation
*/
}
}
