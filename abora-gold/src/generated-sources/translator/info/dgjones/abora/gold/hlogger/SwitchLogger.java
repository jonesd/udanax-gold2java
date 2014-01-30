/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.hlogger;

import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.gchooks.DeleteExecutor;
import info.dgjones.abora.gold.hlogger.SwitchLogger;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.Logger;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class SwitchLogger extends Thunk {

	protected String myLoggerName;
	protected String myDirective;
/*
udanax-top.st:58015:
Thunk subclass: #SwitchLogger
	instanceVariableNames: '
		myLoggerName {char star}
		myDirective {char star}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-hlogger'!
*/
/*
udanax-top.st:58021:
(SwitchLogger getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SwitchLogger.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void execute() {
	(Logger.get(myLoggerName)).init(myDirective);
/*
udanax-top.st:58026:SwitchLogger methodsFor: 'operate'!
{void} execute
	
	(Logger get: myLoggerName) init: myDirective!
*/
}
public void restartSwitchLogger(Rcvr rcvr) {
	DeleteExecutor.registerHolder(this, myLoggerName);
	DeleteExecutor.registerHolder(this, myDirective);
/*
udanax-top.st:58032:SwitchLogger methodsFor: 'hooks:'!
{void} restartSwitchLogger: rcvr {Rcvr unused default: NULL}
	DeleteExecutor registerHolder: self with: myLoggerName.
	DeleteExecutor registerHolder: self with: myDirective.!
*/
}
public SwitchLogger(Rcvr receiver) {
	super(receiver);
	myLoggerName = receiver.receiveString();
	myDirective = receiver.receiveString();
/*
udanax-top.st:58038:SwitchLogger methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myLoggerName _ receiver receiveString.
	myDirective _ receiver receiveString.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendString(myLoggerName);
	xmtr.sendString(myDirective);
/*
udanax-top.st:58043:SwitchLogger methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendString: myLoggerName.
	xmtr sendString: myDirective.!
*/
}
public SwitchLogger() {
/*

Generated during transformation
*/
}
}
