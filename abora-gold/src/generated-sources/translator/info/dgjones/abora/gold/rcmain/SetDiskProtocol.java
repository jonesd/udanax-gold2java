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
import info.dgjones.abora.gold.rcmain.SetDiskProtocol;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * When executed, the receiver will set the disk protocol for the next connection.
 */
public class SetDiskProtocol extends Thunk {

	protected String myName;
/*
udanax-top.st:57779:
Thunk subclass: #SetDiskProtocol
	instanceVariableNames: 'myName {char star}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-rcmain'!
*/
/*
udanax-top.st:57783:
SetDiskProtocol comment:
'When executed, the receiver will set the disk protocol for the next connection.'!
*/
/*
udanax-top.st:57785:
(SetDiskProtocol getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SetDiskProtocol.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Execute the action defined by this thunk.
 */
public void execute() {
	ProtocolBroker.setDiskProtocol((ProtocolBroker.diskProtocol(myName)));
/*
udanax-top.st:57790:SetDiskProtocol methodsFor: 'operate'!
{void} execute
	"Execute the action defined by this thunk."
	ProtocolBroker setDiskProtocol: (ProtocolBroker diskProtocol: myName)!
*/
}
public void restartSetDiskProtocol(Rcvr rcvr) {
	DeleteExecutor.registerHolder(this, myName);
/*
udanax-top.st:57797:SetDiskProtocol methodsFor: 'hooks:'!
{void} restartSetDiskProtocol: rcvr {Rcvr unused default: NULL}
	DeleteExecutor registerHolder: self with: myName!
*/
}
public SetDiskProtocol(Rcvr receiver) {
	super(receiver);
	myName = receiver.receiveString();
/*
udanax-top.st:57802:SetDiskProtocol methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myName _ receiver receiveString.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendString(myName);
/*
udanax-top.st:57806:SetDiskProtocol methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendString: myName.!
*/
}
public SetDiskProtocol() {
/*

Generated during transformation
*/
}
}
