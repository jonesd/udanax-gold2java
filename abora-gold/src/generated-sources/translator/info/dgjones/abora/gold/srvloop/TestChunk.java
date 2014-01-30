/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.srvloop;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.rcmain.ServerChunk;
import info.dgjones.abora.gold.srvloop.TestChunk;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class TestChunk extends ServerChunk {

/*
udanax-top.st:51131:
ServerChunk subclass: #TestChunk
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-srvloop'!
*/
/*
udanax-top.st:51135:
(TestChunk getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(TestChunk.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void processInput() {
/*
udanax-top.st:51140:TestChunk methodsFor: 'accessing'!
{void} processInput!
*/
}
public boolean execute() {
	return false;
/*
udanax-top.st:51144:TestChunk methodsFor: 'execute'!
{BooleanVar} execute
	^ false!
*/
}
public TestChunk() {
/*

Generated during transformation
*/
}
public TestChunk(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
