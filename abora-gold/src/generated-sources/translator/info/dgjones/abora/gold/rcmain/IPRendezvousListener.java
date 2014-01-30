/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.rcmain;

import info.dgjones.abora.gold.java.AboraSocketSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.UnixSocketAccessor;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.rcmain.FDListener;
import info.dgjones.abora.gold.rcmain.IPPromiseListener;
import info.dgjones.abora.gold.rcmain.IPRendezvousListener;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * An IPRendezvousListener binds to a known rendezvous socket address.
 * Its handleInput method accepts connection on this socket and sets up a FEBE connection
 * on the spawned socket, including a IPConnectionListener.
 */
public class IPRendezvousListener extends FDListener {

	protected int myAddress;
/*
udanax-top.st:50994:
FDListener subclass: #IPRendezvousListener
	instanceVariableNames: 'myAddress {UInt32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-rcmain'!
*/
/*
udanax-top.st:50998:
IPRendezvousListener comment:
'An IPRendezvousListener binds to a known rendezvous socket address.
	Its handleInput method accepts connection on this socket and sets up a FEBE connection
	on the spawned socket, including a IPConnectionListener.'!
*/
/*
udanax-top.st:51002:
(IPRendezvousListener getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:51119:
IPRendezvousListener class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:51122:
(IPRendezvousListener getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(COPY boot ); add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IPRendezvousListener.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"COPY", "boot"}).add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public void restartIPRendezvous(Rcvr rcvr) {
	int aSocket;
	AboraSupport.smalltalkOnly();
	{
		aSocket = UnixSocketAccessor.newTCPserverAtPort(myAddress);
		AboraSocketSupport.listenFor(aSocket, 5);
	}
	AboraSupport.translateOnly();
	{
		/* 
#ifdef NETMNG
#define SOCKA_SIZE sizeof(struct sockaddr)
	struct	sockaddr * sockName = (struct sockaddr *)falloc(SOCKA_SIZE);	
#else
#define SOCKA_SIZE sizeof(struct sockaddr_in)
	struct	sockaddr_in * sockName = (struct sockaddr_in *)falloc(SOCKA_SIZE);
#endif	
#ifdef UndeFIned_FALSE
#define SOCKA_SIZE sizeof(struct sockaddr)
	struct	sockaddr * sockName = (struct sockaddr *)falloc(SOCKA_SIZE);
#endif
#ifdef GNU
#define SOCK_CAST 
#else
#define SOCK_CAST (sockaddr *)
#endif

	sockName->sin_family = AF_INET;
	sockName->sin_port = htons((unsigned short)myAddress);
	sockName->sin_addr.s_addr = INADDR_ANY;
	
	aSocket = socket (PF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (aSocket < 0) {
		BLAST(CANT_OPEN_RENDEZVOUS_SOCKET);
	}

	if (bind ( aSocket, SOCK_CAST sockName, SOCKA_SIZE) < 0) {

		BLAST(CANT_BIND_RENDEZVOUS_SOCKET);
	}
	if (listen (aSocket, 5) < 0) {
		BLAST(SOCKET_LISTEN_FAILED);
	}
 */
	}
	registerFor(aSocket);
/*
udanax-top.st:51007:IPRendezvousListener methodsFor: 'hooks:'!
{void RECEIVE.HOOK} restartIPRendezvous: rcvr {Rcvr unused default: NULL}
	| aSocket {int} |
	[aSocket _ UnixSocketAccessor newTCPserverAtPort: myAddress.
	aSocket listenFor: 5] smalltalkOnly.
	
	'
#ifdef NETMNG
#define SOCKA_SIZE sizeof(struct sockaddr)
	struct	sockaddr * sockName = (struct sockaddr *)falloc(SOCKA_SIZE);	
#else
#define SOCKA_SIZE sizeof(struct sockaddr_in)
	struct	sockaddr_in * sockName = (struct sockaddr_in *)falloc(SOCKA_SIZE);
#endif	
#ifdef UndeFIned_FALSE
#define SOCKA_SIZE sizeof(struct sockaddr)
	struct	sockaddr * sockName = (struct sockaddr *)falloc(SOCKA_SIZE);
#endif
#ifdef GNU
#define SOCK_CAST 
#else
#define SOCK_CAST (sockaddr *)
#endif
	sockName->sin_family = AF_INET;
	sockName->sin_port = htons((unsigned short)myAddress);
	sockName->sin_addr.s_addr = INADDR_ANY;
	
	aSocket = socket (PF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (aSocket < 0) {
		BLAST(CANT_OPEN_RENDEZVOUS_SOCKET);
	}
	if (bind ( aSocket, SOCK_CAST sockName, SOCKA_SIZE) < 0) {
		BLAST(CANT_BIND_RENDEZVOUS_SOCKET);
	}
	if (listen (aSocket, 5) < 0) {
		BLAST(SOCKET_LISTEN_FAILED);
	}
' translateOnly.
	
	self registerFor: aSocket!
*/
}
public IPRendezvousListener(int anAddress) {
	super();
	myAddress = anAddress;
	restartIPRendezvous(null);
/*
udanax-top.st:51054:IPRendezvousListener methodsFor: 'creation'!
create: anAddress {UInt32}
	super create.
	myAddress _ anAddress.
	
	self restartIPRendezvous: NULL!
*/
}
/**
 * A client is trying to connect to the rendezvous socket.
 * Accept the connection and spawn an IPconnectionListener for them.
 * NOTE: in smalltalk (only) it is not guarnteed that there is anyone there.
 * so we do a non blocking operation and return quietly if there isn't
 */
public boolean execute() {
	int newSocket;
	CurrentChunk = this;
	AboraSupport.smalltalkOnly();
	{
		newSocket = AboraSocketSupport.acceptNonBlock(super.descriptor());
		if (newSocket == 0) {
			return false;
		}
	}
	AboraSupport.translateOnly();
	{
		/* 
#ifdef GNU
	sockaddr_in fromAddr;
#else
	sockaddr fromAddr;
#endif
	int fromAddrLen = sizeof fromAddr;
#ifdef NETMNG
	newSocket = accept (this->descriptor(), &fromAddr, &fromAddrLen);
#else
	newSocket = accept (this->descriptor(), (sockaddr*)&fromAddr, &fromAddrLen);
#endif
	if (newSocket < 0) {
		CurrentChunk = NULL;
		BLAST(ACCEPT_FAILURE_ON_RENDEZVOUS_SOCKET);
	}
 */
	}
	try {
		IPPromiseListener.make(newSocket);
	}
	catch (AboraRuntimeException ex) {
		if (AboraRuntimeException.SOCKETURECVUERROR.equals(ex.getMessage()) || AboraRuntimeException.SOCKETUSENDUERROR.equals(ex.getMessage())) {
			AboraSupport.translateOnly();
			{
				// /*cerr << &PROBLEM(ex);*/
			}
			AboraSupport.translateOnly();
			{
				/* operator<<(cerr,(Problem *)&PROBLEM(ex)); */
			}
			AboraSupport.logger.print(" Connection aborted.\n"+
"");
			CurrentChunk = null;
			return false;
		}
		else {
			throw ex;
		}
	}
	CurrentChunk = null;
	return false;
/*
udanax-top.st:51063:IPRendezvousListener methodsFor: 'accessing'!
{BooleanVar} execute
	"A client is trying to connect to the rendezvous socket.
Accept the connection and spawn an IPconnectionListener for them.
NOTE: in smalltalk (only) it is not guarnteed that there is anyone there.
so we do a non blocking operation and return quietly if there isn't"
	| newSocket {int} |
	[IPPromiseListener] USES.
	CurrentChunk _ self.
	[newSocket _ super descriptor acceptNonBlock.
	newSocket = nil ifTrue: [ ^false ]] smalltalkOnly.
	'
#ifdef GNU
	sockaddr_in fromAddr;
#else
	sockaddr fromAddr;
#endif
	int fromAddrLen = sizeof fromAddr;
#ifdef NETMNG
	newSocket = accept (this->descriptor(), &fromAddr, &fromAddrLen);
#else
	newSocket = accept (this->descriptor(), (sockaddr*)&fromAddr, &fromAddrLen);
#endif
	if (newSocket < 0) {
		CurrentChunk = NULL;
		BLAST(ACCEPT_FAILURE_ON_RENDEZVOUS_SOCKET);
	}
' translateOnly.
	FDListener problems.SOCKET.U.ERRS 
		handle: [:ex | 
				'/-cerr << &PROBLEM(ex);-/' translateOnly.
				'operator<<(cerr,(Problem *)&PROBLEM(ex));'translateOnly.
				cerr << ' Connection aborted.
'.
				CurrentChunk _ NULL.
				^false]
		do: 	[IPPromiseListener make: newSocket].
	CurrentChunk _ NULL.
	^false!
*/
}
public boolean shouldBeReady() {
	return true;
/*
udanax-top.st:51103:IPRendezvousListener methodsFor: 'accessing'!
{BooleanVar} shouldBeReady
	^true "since this is not really a connection it is always OK"!
*/
}
public IPRendezvousListener(Rcvr receiver) {
	super(receiver);
	myAddress = receiver.receiveUInt32();
	restartIPRendezvous(receiver);
/*
udanax-top.st:51109:IPRendezvousListener methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myAddress _ receiver receiveUInt32.
	self restartIPRendezvous: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendUInt32(myAddress);
/*
udanax-top.st:51114:IPRendezvousListener methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendUInt32: myAddress.!
*/
}
public static FDListener make(int anAddress) {
	return new IPRendezvousListener(anAddress);
/*
udanax-top.st:51127:IPRendezvousListener class methodsFor: 'creation'!
{FDListener} make: anAddress {UInt32}
	^self create: anAddress.!
*/
}
public IPRendezvousListener() {
/*

Generated during transformation
*/
}
}
