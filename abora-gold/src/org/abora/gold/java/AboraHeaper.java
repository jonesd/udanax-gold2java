/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java;

import java.io.PrintWriter;
import java.io.StringWriter;

import org.abora.gold.be.basic.BeGrandMap;
import org.abora.gold.be.basic.ID;
import org.abora.gold.be.canopy.BertCrum;
import org.abora.gold.be.canopy.CanopyCache;
import org.abora.gold.cobbler.Connection;
import org.abora.gold.collection.sets.MuSet;
import org.abora.gold.java.exception.SubclassResponsibilityException;
import org.abora.gold.java.missing.Developer;
import org.abora.gold.java.missing.OrderEnum;
import org.abora.gold.java.missing.smalltalk.AboraClass;
import org.abora.gold.nadmin.FeSession;
import org.abora.gold.nkernel.FeKeyMaster;
import org.abora.gold.nkernel.FeServer;
import org.abora.gold.rcmain.ServerChunk;
import org.abora.gold.snarf.DiskManager;
import org.abora.gold.spaces.integers.IntegerPos;
import org.abora.gold.traces.TracePosition;
import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xcvr.Recipe;
import org.abora.gold.xpp.basic.Category;
import org.abora.gold.xpp.basic.Heaper;
import org.abora.gold.xpp.fluid.FluidVar;
import org.abora.gold.xpp.fluid.GlobalEmulsion;

public class AboraHeaper {
	//TODO these probably shouldn't be globals...
	public static FluidVar ActiveClubs = new FluidVar(null, GlobalEmulsion.make(), "ACTIVE_CLUBS", AboraSupport.findAboraClass(MuSet.class));
	public static FluidVar CurrentAuthor = new FluidVar(null, GlobalEmulsion.make(), "CURRENT_AUTHOR", AboraSupport.findAboraClass(ID.class));
	public static FluidVar CurrentBertCanopyCache = new FluidVar(null, GlobalEmulsion.make(), "CURRENT_BERT_CANOPY_CACHE", AboraSupport.findAboraClass(CanopyCache.class));
	public static FluidVar CurrentBertCrum = new FluidVar(null, GlobalEmulsion.make(), "CURRENT_BERT_CRUM", AboraSupport.findAboraClass(BertCrum.class));
	public static FluidVar CurrentGrandMap = new FluidVar(null, GlobalEmulsion.make(), "CURRENT_GRAND_MAP", AboraSupport.findAboraClass(BeGrandMap.class));
	public static FluidVar CurrentKeyMaster = new FluidVar(null, GlobalEmulsion.make(), "CURRENT_KEY_MASTER", AboraSupport.findAboraClass(FeKeyMaster.class));
	public static FluidVar CurrentMainReceiver;
	public static FluidVar CurrentPacker = new FluidVar(null, GlobalEmulsion.make(), "CURRENT_PACKER", AboraSupport.findAboraClass(DiskManager.class));
	public static FluidVar CurrentSensorCanopyCache = new FluidVar(null, GlobalEmulsion.make(), "CURRENT_SENSOR_CANOPY_CACHE", AboraSupport.findAboraClass(CanopyCache.class));
	public static FluidVar CurrentServer = new FluidVar(null, GlobalEmulsion.make(), "CURRENT_SERVER", AboraSupport.findAboraClass(FeServer.class));
	public static FluidVar CurrentServerConnection;
	public static FluidVar CurrentServerLoop;
	public static FluidVar CurrentSession = new FluidVar(null, GlobalEmulsion.make(), "CURRENT_SESSION", AboraSupport.findAboraClass(FeSession.class));
	public static FluidVar CurrentSessions;
	public static FluidVar CurrentTrace= new FluidVar(null, GlobalEmulsion.make(), "CURRENT_TRACE", AboraSupport.findAboraClass(TracePosition.class));
	public static FluidVar GrandConnection = new FluidVar(null, GlobalEmulsion.make(), "GRAND_CONNECTION", AboraSupport.findAboraClass(Connection.class));
	public static FluidVar InitialEditClub = new FluidVar(null, GlobalEmulsion.make(), "INITIAL_EDIT_CLUB", AboraSupport.findAboraClass(ID.class));
	public static FluidVar InitialOwner = new FluidVar(null, GlobalEmulsion.make(), "INITIAL_OWNER", AboraSupport.findAboraClass(ID.class));
	public static FluidVar InitialReadClub = new FluidVar(null, GlobalEmulsion.make(), "INITIAL_READ_CLUB", AboraSupport.findAboraClass(ID.class));
	public static FluidVar InitialSponsor = new FluidVar(null, GlobalEmulsion.make(), "INITIAL_SPONSOR", AboraSupport.findAboraClass(ID.class));
	public static FluidVar InsideTransactionFlag = new FluidVar(null, GlobalEmulsion.make(), "INSIDE_TRANSACTION_FLAG", AboraSupport.findAboraClass(Boolean.class));
	public static FluidVar InsideAgenda = new FluidVar(null, GlobalEmulsion.make(), "INSIDE_AGENDA", AboraSupport.findAboraClass(Boolean.class));
	public static FluidVar MainActiveThunk;
	
//	public static Recipe BootCuisine;
//	public static Recipe CalcCuisine;
//	public static Recipe DiskCuisine;
//	public static Recipe FebeCuisine;
//	public static Recipe XppCuisine;
	
	public static ServerChunk CurrentChunk;
	
	// Developer notes
	public static final Developer Dean = new Developer("Dean");
	public static final Developer Eric = new Developer("Eric");
	public static final Developer MarkM = new Developer("MarkM");
	public static final Developer Ravi = new Developer("Ravi");
	public static final Developer RaviNow = new Developer("RaviNow");
	public static final Developer Someone = new Developer("Someone");

	//TODO what to really do with these, often Smalltalk Symbols?
	public static final String ALL_TESTS_ON_ = "AllTestsOn";
	public static final String BLAST_LOG = "BLAST_LOG";
	public static final String CHECK_ = "check";
	public static final String CONCRETE = "CONCRETE";
	public static final String COPY = "COPY";
	public static final String CURRENT_CHUNK = "CurrentChunk";
	public static final String DONT_CHANGE_TURTLES_BOOT_HEAPER = "DontChangeTurtlesBootHeaper";
	public static final String EDITION_UWITH_ALL_UN3_WITH_WITH_ = "EDITION_UWITH_ALL_UN3_WITH_WITH_";
	public static final String FOO_LOG = "FOO_LOG";
	public static final String MAKE_WRAPPER_ = "makeWrapper";
	public static final String MANUALRECIPE = "MANUALRECIPE";
	public static final String NO_ENCRYPTER = "org.abora.gold.lock.NoEncrypter";
	public static final String NO_REQUEST_ = "NO_REQUEST_";
	public static final String PACKAGE = "PACKAGE";
	public static final String PACKAGEHOOK = "PACKAGEHOOK";
	public static final String PSEUDOCOPY = "PSEUDOCOPY";
	public static final String SET_SPEC_ = "setSpec";
	public static final String INIT_IMAGE_EMULSION = "initImageEmulsion";
	
	public static final String PUBLIC = "public";
	public static final String PRIVATE = "private";
	public static final String PROTECTED = "protected";
	public static final String TEST = "test";
	
	public static final String DIR = "DIR";
	public static final String FILE = "FILE";
	
	public static final String BOOT_CUISINE = "BootCuisine";
	public static final String CALC_CUISINE = "CalcCuisine";
	public static final String DISK_CUISINE = "DiskCuisine";
	public static final String FEBE_CUISINE = "FebeCuisine";
	public static final String XPP_CUISINE = "XppCuisine";
	
	public static final String FE_BOO_LOCK_SMITH = "org.abora.gold.nadmin.FeBooLockSmith";
	public static final String FE_CHALLENGE_LOCK_SMITH = "org.abora.gold.nadmin.FeChallengeLockSmith";
	public static final String FE_CLUB_DESCRIPTION = "org.abora.gold.nadmin.FeClubDescription";
	public static final String FE_HYPER_LINK = "org.abora.gold.nlinks.FeHyperLink";
	public static final String FE_HYPER_REF = "org.abora.gold.nlinks.FeHyperRef";
	public static final String FE_LOCK_SMITH = "org.abora.gold.nadmin.FeLockSmith";
	public static final String FE_MATCH_LOCK_SMITH = "org.abora.gold.nadmin.FeMatchLockSmith";
	public static final String FE_MULTI_LOCK_SMITH = "org.abora.gold.nadmin.FeMultiLockSmith";
	public static final String FE_MULTI_REF = "org.abora.gold.nlinks.FeMultiRef";
	public static final String FE_PATH = "org.abora.gold.nlinks.FePath";
	public static final String FE_SET = "org.abora.gold.wrapper.FeSet";
	public static final String FE_SINGLE_REF = "org.abora.gold.nlinks.FeSingleRef";
	public static final String FE_TEXT = "org.abora.gold.wrapper.FeText";
	public static final String FE_WALL_LOCK_SMITH = "org.abora.gold.nadmin.FeWallLockSmith";
	public static final String FE_WORK_SET = "org.abora.gold.wrapper.FeWorkSet";
	public static final String FE_WRAPPER = "org.abora.gold.wrapper.FeWrapper";
	
	public static final OrderEnum EQUAL = null;
	public static final OrderEnum GREATERUTHAN = null;
	public static final OrderEnum LESSUTHAN = null;
	public static final OrderEnum INCOMPARABLE = null;
	
	public static final IntegerPos ZERO = null;
	public static final IntegerPos ONE = null;
	public static final IntegerPos TWO = null;
	public static final IntegerPos THREE = null;
	
	public static final PrintWriter BlastLog = null;
	public static final PrintWriter ErrorLog = null;
	public static final PrintWriter FooLog = null;
	public static final PrintWriter VanillaLog = null;

	/**
	 * Constructor for AboraHeaper.
	 */
	public AboraHeaper() {
		super();
	}
	public AboraHeaper(Rcvr receiver) {
		throw new UnsupportedOperationException();
	}
	

	public static void shouldNotImplement() {
		throw new UnsupportedOperationException();
	}
	public static void error(String message) {
		throw new UnsupportedOperationException();
	}
	public static void willNotImplement() {
		throw new UnsupportedOperationException();
	}
	public static void mightNotImplement() {
		throw new UnsupportedOperationException();
	}
	public static void REQUIRES(Category cat) {
		throw new UnsupportedOperationException();
	}
	public void stubbleForSubclassResponsibility() {
		throw new UnsupportedOperationException();
	}
	public int halt() {
		throw new UnsupportedOperationException();
	}
	public int asOop() {
		//TODO wrong!
		return System.identityHashCode(this);
		//throw new UnsupportedOperationException();
	}
	public Object inspect() {
		throw new UnsupportedOperationException();
	}
	public void basicInspect() {
		throw new UnsupportedOperationException();
	}
	public String displayString() {
		throw new UnsupportedOperationException();
	}
	
	// Temp replace Heaper implementation
	public static int takeOop() {
		throw new UnsupportedOperationException();
	}
	
	// Temp replace Heaper implementation
	public Category getCategory() {
		//TODO something more?
		return AboraSupport.findCategory(this.getClass());
	}
	
	// Temp replace Heaper implementation
	public void delete() {
		//TODO Should do something here!
	}

	// Temp replace Heaper implementation
	public void destructor() {
		//TODO Should do something here!
	}
	
	// Temp replace Heaper implementation
	public static void setGC(boolean a) {
		throw new UnsupportedOperationException();
	}
	public String printString() {
		StringWriter stringWriter = new StringWriter();
		PrintWriter writer = new PrintWriter(stringWriter);
		printOn(writer);
		writer.flush();
		return stringWriter.toString();
	}
	public void markInstances(int i) {
		throw new UnsupportedOperationException();
	}
	public static void mayBecome(Category cat) {
		throw new UnsupportedOperationException();
	}
	
	public boolean isEqualOrSubclassOf(Category cat) {
		//TODO should be added to Category?
		throw new UnsupportedOperationException();
	}
	public boolean isKindOf(Category cat) {
		Category category = getCategory();
		//TODO should we really use category.inheritsFrom(...) instead?
		return category.brotherClass().inheritsFrom(cat.brotherClass());
	}
	public static int preorderMax() {
		//TODO made up number!
		//return Heaper.classHierarchy().length;
		return 1000;
	}
	public void fixup() {
		throw new UnsupportedOperationException();
	}
	public static Fn pointerToStaticMember(String name, String handlerType) {
		throw new UnsupportedOperationException();
	}
	public static Fn pointerToStaticMember(String name) {
		throw new UnsupportedOperationException();
	}
	
	public String toString() {
		return printString();
	}
	
	public void printOn(PrintWriter oo) {
		throw new SubclassResponsibilityException();
	}
	public AboraClass getAboraClass() {
		return AboraSupport.findAboraClass(getClass());
	}

}
