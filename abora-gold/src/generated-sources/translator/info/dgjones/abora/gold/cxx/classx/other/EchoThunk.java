/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cxx.classx.other;

import info.dgjones.abora.gold.cxx.classx.other.EchoThunk;
import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.gchooks.DeleteExecutor;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class EchoThunk extends Thunk {

	protected String message;
/*
udanax-top.st:57263:
Thunk subclass: #EchoThunk
	instanceVariableNames: 'message {char star}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cxx-class-other'!
*/
/*
udanax-top.st:57267:
(EchoThunk getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(EchoThunk.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Execute the action defined by this thunk.
 */
public void execute() {
	AboraSupport.logger.print(message);
	AboraSupport.logger.print("\n"+
"");
/*
udanax-top.st:57272:EchoThunk methodsFor: 'action'!
{void} execute
	"Execute the action defined by this thunk."
	
	cerr << message << '
'!
*/
}
public void restartEchoThunk(Rcvr rcvr) {
	DeleteExecutor.registerHolder(this, message);
/*
udanax-top.st:57280:EchoThunk methodsFor: 'hooks:'!
{void} restartEchoThunk: rcvr {Rcvr unused default: NULL}
	DeleteExecutor registerHolder: self with: message!
*/
}
public EchoThunk(Rcvr receiver) {
	super(receiver);
	message = receiver.receiveString();
/*
udanax-top.st:57285:EchoThunk methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	message _ receiver receiveString.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendString(message);
/*
udanax-top.st:57289:EchoThunk methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendString: message.!
*/
}
public EchoThunk() {
/*

Generated during transformation
*/
}
}
