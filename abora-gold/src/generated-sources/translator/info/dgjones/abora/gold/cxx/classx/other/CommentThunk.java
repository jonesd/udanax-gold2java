/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cxx.classx.other;

import info.dgjones.abora.gold.cxx.classx.other.CommentThunk;
import info.dgjones.abora.gold.fm.support.Thunk;
import info.dgjones.abora.gold.gchooks.DeleteExecutor;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class CommentThunk extends Thunk {

	protected String message;
/*
udanax-top.st:57164:
Thunk subclass: #CommentThunk
	instanceVariableNames: 'message {char star}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cxx-class-other'!
*/
/*
udanax-top.st:57168:
(CommentThunk getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CommentThunk.class).setAttributes( new Set().add("CONCRETE").add("COPY").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void restartCommentThunk(Rcvr rcvr) {
	DeleteExecutor.registerHolder(this, message);
/*
udanax-top.st:57173:CommentThunk methodsFor: 'hooks:'!
{void} restartCommentThunk: rcvr {Rcvr unused default: NULL}
	DeleteExecutor registerHolder: self with: message.!
*/
}
public void execute() {
/*
udanax-top.st:57178:CommentThunk methodsFor: 'action'!
{void} execute!
*/
}
public CommentThunk(Rcvr receiver) {
	super(receiver);
	message = receiver.receiveString();
/*
udanax-top.st:57182:CommentThunk methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	message _ receiver receiveString.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendString(message);
/*
udanax-top.st:57186:CommentThunk methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendString: message.!
*/
}
public CommentThunk() {
/*

Generated during transformation
*/
}
}
