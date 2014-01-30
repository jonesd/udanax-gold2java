/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.testing;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.testing.HelloTester;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class HelloTester extends Tester {

/*
udanax-top.st:59109:
Tester subclass: #HelloTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Testing'!
*/
/*
udanax-top.st:59113:
(HelloTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(HelloTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public int ifdefTest() {
	return 0;
/*
udanax-top.st:59118:HelloTester methodsFor: 'testing'!
{IntegerVar ifdefFOO ifndefBAR} ifdefTest
 
	^IntegerVar0!
*/
}
/**
 * self tryTest: #test1On:
 */
public void test1On(PrintWriter aStream) {
	aStream.print("Hello, translated world!\n"+
"");
/*
udanax-top.st:59122:HelloTester methodsFor: 'testing'!
{void} test1On: aStream {ostream reference} 
	"self tryTest: #test1On:"
	
	aStream << 'Hello, translated world!!
'!
*/
}
/**
 * HelloTester runTest
 */
public void allTestsOn(PrintWriter aStream) {
	aStream.print(" \n"+
"Running Hello, world! test.\n"+
"");
	test1On(aStream);
/*
udanax-top.st:59130:HelloTester methodsFor: 'running tests'!
{void} allTestsOn: aStream {ostream reference} 
	"HelloTester runTest"
	
	aStream << ' 
Running Hello, world!! test.
'.
	self test1On: aStream.!
*/
}
public HelloTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:59140:HelloTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:59143:HelloTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public HelloTester() {
/*

Generated during transformation
*/
}
}
