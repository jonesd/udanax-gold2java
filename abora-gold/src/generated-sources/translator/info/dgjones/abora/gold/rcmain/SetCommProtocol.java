/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.rcmain;

import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.gchooks.DeleteExecutor;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.negoti8.ProtocolBroker;
import info.dgjones.abora.gold.rcmain.SetCommProtocol;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * When executed, the receiver will set the comm protocol for the next connection.
 */
public class SetCommProtocol extends Thunk {

	protected String myName;
/*
udanax-top.st:57748:
Thunk subclass: #SetCommProtocol
	instanceVariableNames: 'myName {char star}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-rcmain'!
*/
/*
udanax-top.st:57752:
SetCommProtocol comment:
'When executed, the receiver will set the comm protocol for the next connection.'!
*/
/*
udanax-top.st:57754:
(SetCommProtocol getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SetCommProtocol.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void restartSetCommProtocol(Rcvr rcvr) {
	DeleteExecutor.registerHolder(this, myName);
/*
udanax-top.st:57759:SetCommProtocol methodsFor: 'hooks:'!
{void} restartSetCommProtocol: rcvr {Rcvr unused default: NULL}
	DeleteExecutor registerHolder: self with: myName!
*/
}
/**
 * Execute the action defined by this thunk.
 */
public void execute() {
	ProtocolBroker.setCommProtocol((ProtocolBroker.commProtocol(myName)));
/*
udanax-top.st:57764:SetCommProtocol methodsFor: 'thunking'!
{void} execute
	"Execute the action defined by this thunk."
	ProtocolBroker setCommProtocol: (ProtocolBroker commProtocol: myName)!
*/
}
public SetCommProtocol(Rcvr receiver) {
	super(receiver);
	myName = receiver.receiveString();
/*
udanax-top.st:57771:SetCommProtocol methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myName _ receiver receiveString.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendString(myName);
/*
udanax-top.st:57775:SetCommProtocol methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendString: myName.!
*/
}
public SetCommProtocol() {
/*

Generated during transformation
*/
}
}
