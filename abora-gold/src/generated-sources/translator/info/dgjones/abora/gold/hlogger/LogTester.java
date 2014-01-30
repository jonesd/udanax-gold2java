/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.hlogger;

import info.dgjones.abora.gold.hlogger.LogTester;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.Logger;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.testing.Tester;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class LogTester extends Tester {

/*
udanax-top.st:59523:
Tester subclass: #LogTester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-hlogger'!
*/
/*
udanax-top.st:59527:
(LogTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
/*
udanax-top.st:59560:
LogTester class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:59563:
(LogTester getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(LogTester.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}));
/*

Generated during transformation: AddMethod
*/
}
public void allTestsOn(PrintWriter oo) {
	FooLog.print("foo ");
	FooLog.print(this);
	FooLog.print("\n"+
"");
	VanillaLog.print("bar ");
	VanillaLog.print(this);
	VanillaLog.print("\n"+
"");
	ErrorLog.print("err ");
	ErrorLog.print(this);
	ErrorLog.print("\n"+
"");
	PrintWriter ooo = FooLog;
	ooo.print("zip ");
	ooo.print(this);
	ooo.print("\n"+
"");
	PrintWriter ooo1 = VanillaLog;
	ooo1.print("zap ");
	ooo1.print(this);
	ooo1.print("\n"+
"");
	PrintWriter ooo2 = ErrorLog;
	ooo2.print("human ");
	ooo2.print(this);
	ooo2.print("\n"+
"");
/*
udanax-top.st:59532:LogTester methodsFor: 'testing'!
{void} allTestsOn: oo {ostream reference} 
 
	FooLog << 'foo ' << self << '
'.
	VanillaLog << 'bar ' << self << '
'.
	ErrorLog << 'err ' << self << '
'.
	FooLog LOG: [:ooo |
		ooo << 'zip ' << self << '
'].
	VanillaLog LOG: [:ooo |
		ooo << 'zap ' << self << '
'].
	ErrorLog LOG: [:ooo |
		ooo << 'human ' << self << '
'].!
*/
}
public LogTester(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:59553:LogTester methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:59556:LogTester methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static void linkTimeNonInherited() {
	Logger.defineLogger(FOO_LOG);
/*
udanax-top.st:59568:LogTester class methodsFor: 'smalltalk: init'!
linkTimeNonInherited
	Logger defineLogger: #FooLog!
*/
}
public LogTester() {
/*

Generated during transformation
*/
}
}
