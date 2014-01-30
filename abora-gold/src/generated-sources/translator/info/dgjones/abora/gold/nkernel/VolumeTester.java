/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.nkernel;

import info.dgjones.abora.gold.cobbler.Connection;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.VolumeTester;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class VolumeTester extends Tester {

	protected Connection myConnection;
/*
udanax-top.st:62062:
Tester subclass: #VolumeTester
	instanceVariableNames: 'myConnection {Connection NOCOPY}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-nkernel'!
*/
/*
udanax-top.st:62066:
(VolumeTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #SMALLTALK.ONLY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(VolumeTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("SMALLTALKONLY"));
/*

Generated during transformation: AddMethod
*/
}
public void allTestsOn(PrintWriter oo) {
	/* Transform: Convert code later */
	throw new UnsupportedOperationException("Implement later");
/*
udanax-top.st:62071:VolumeTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
	| server file protoArray count chunk result  stepper |
	Dean shouldImplement. "bring this up to date"
	
	myConnection := Connection make: FeServer.
	server _ myConnection bootHeaper.
	
	protoArray _ UInt8Array basicNew.
	file _ (Filename named: 'base.cha') readStream.
	result _ FeEdition empty: IntegerSpace make.
	
	[count _ 0.
	chunk _ 41000.
	50 timesRepeat:
		[| data {UInt8Array} |
		oo << count << '
'.
		data _ file next: chunk.
		data changeClassToThatOf: protoArray.
		result _ result combine: 
			((server newEdition: data) transformedBy: (IntegerMapping make: count)).
		count _ count + chunk].
	result domain.
	stepper _ RandomStepper make: 389.
	
	120 timesRepeat: 
		[| start {IntegerVar} stop {IntegerVar} region {XnRegion} |
		start _ stepper value \\ count.
		stepper step.
		stop _ stepper value \\ count.
		stepper step.
		region _ IntegerRegion make: start with: stop.
		oo << region << '
'.
		result copy: region].
	
	200 timesRepeat: 
		[| start {IntegerVar} stop {IntegerVar} region {XnRegion} |
		start _ stepper value \\ count.
		stepper step.
		stop _ stepper value \\ 30000.
		stop > count ifTrue: [stop _ count].
		stepper step.
		region _ IntegerRegion make: start with: stop.
		oo << region << '
'.
		result retrieve: region with: NULL]]
	valueNowOrOnUnwindDo: [file close]!
*/
}
public VolumeTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:62122:VolumeTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:62125:VolumeTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public VolumeTester() {
/*

Generated during transformation
*/
}
}
