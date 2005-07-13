package org.abora.gold.java;

import java.io.PrintWriter;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.abora.gold.be.basic.BeClub;
import org.abora.gold.be.basic.BeGrandMap;
import org.abora.gold.be.canopy.BertCrum;
import org.abora.gold.be.canopy.SensorCrum;
import org.abora.gold.be.ents.Ent;
import org.abora.gold.cxx.classx.comm.CategoryRecipe;
import org.abora.gold.fbtest.WorksBootMaker;
import org.abora.gold.java.missing.smalltalk.AboraClass;
import org.abora.gold.java.missing.smalltalk.OrderedCollection;
import org.abora.gold.nadmin.FePromiseSession;
import org.abora.gold.nadmin.FeSession;
import org.abora.gold.nkernel.FeServer;
import org.abora.gold.nkernel.WorksTester;
import org.abora.gold.rcmain.MainDummy;
import org.abora.gold.rcmain.ServerLoop;
import org.abora.gold.snarf.Abraham;
import org.abora.gold.snarf.DiskManager;
import org.abora.gold.snarf.FlockInfo;
import org.abora.gold.xcvr.Recipe;
import org.abora.gold.xpp.basic.Category;
import org.abora.gold.xpp.basic.Heaper;



public class AboraSupport {

	public static PrintWriter logger = null; 
	
	private static final Map categories = new HashMap();
	private static final Map aboraClasses = new HashMap();
	
	private static final Map globals = new HashMap();
	
	private static boolean initialized = false;
	
	public AboraSupport() {
		super();
	}

	public static Category findCategory(Class c) {
		Category category = (Category)categories.get(c);
		if (category == null) {
			AboraClass aboraClass = findAboraClass(c);
			category = new Category(aboraClass);
			categories.put(c, category);
		}
		return category;
	}
	
	public static AboraClass findAboraClass(Class c) {
		AboraClass aboraClass = (AboraClass)aboraClasses.get(c);
		if (aboraClass == null) {
			aboraClass = new AboraClass(c);
			aboraClasses.put(c, aboraClass);
		}
		return aboraClass;
	}
	
	public static void smalltalkOnly() {
		throw new UnsupportedOperationException();
	}
	
	public static void translateOnly() {
		throw new UnsupportedOperationException();
	}
	
	public static PrintWriter getPrintWriter() {
		throw new UnsupportedOperationException();
	}

	public static void defineFluid(Class class1, String name, Object value, Object value2) {
		//TODO do something here!
		System.out.println("Ignoring defineFluid:"+name);
	}

	public static int xuTime() {
		//TODO absolutely wrong!!!! long -> int
		return (int)(System.currentTimeMillis() / 1000);
	}
	public static int exponent(float f) {
		throw new UnsupportedOperationException();
	}
	public static int exponent(double d) {
		throw new UnsupportedOperationException();
	}

	public static boolean isANumber(double myValue) {
		throw new UnsupportedOperationException();
	}
	public static boolean isANumber(float myValue) {
		throw new UnsupportedOperationException();
	}
	public static OrderedCollection allSubclasses(Class c) {
		throw new UnsupportedOperationException();
	}

	public static OrderedCollection subclasses(Class class1) {
		throw new UnsupportedOperationException();
	}
	public static OrderedCollection subclasses(AboraClass class1) {
		throw new UnsupportedOperationException();
	}

	public static OrderedCollection allInstances(Class class1) {
		throw new UnsupportedOperationException();
	}

	public static int pow(int i, int j) {
		//TODO review
		return (int)Math.pow(i, j);
	}

	public static double pow(double d, float f) {
		//TODO review
		return Math.pow(d, f);
	}

	public static String readStream(String string) {
		throw new UnsupportedOperationException();
	}

	public static String asCapitalized(String cuisine) {
		throw new UnsupportedOperationException();
	}
	
	/**
	 * Smalltalk implementation of modulo - handles negative
	 * and rounding different from Java.
	 */
	public static int modulo(int dividend, int divisor) {
		return dividend - (quotient(dividend, divisor) * divisor);
	}
	
	/**
	 * Smalltalk implementation of quotient - rounds towards negative
	 * infinity, rather than the Java / operatior which rounds towards 0.
	 */
	public static int quotient(int dividend, int divisor) {
		int q = dividend / divisor;
		//TODO is this madness?
		if (((dividend < 0  && divisor > 0) || (dividend > 0 && divisor < 0)) && dividend % divisor != 0) {
			q -= 1;
		}
		return q;
	}

	public static void defineGlobal(String s, Heaper h) {
		//TODO review
		globals.put(s, h);
	}
	
	public static void initializeSystem() {
		if (initialized) {
			return;
		}
		
		BeClub.staticTimeNonInherited();
		BeGrandMap.staticTimeNonInherited();
		BertCrum.staticTimeNonInherited();
		SensorCrum.staticTimeNonInherited();
		Ent.staticTimeNonInherited();
		//TODOCategoryRecipe.staticTimeNonInherited();
		WorksBootMaker.staticTimeNonInherited();
		FePromiseSession.staticTimeNonInherited();
		FeSession.staticTimeNonInherited();
		FeServer.staticTimeNonInherited();
		WorksTester.staticTimeNonInherited();
		MainDummy.staticTimeNonInherited();
		ServerLoop.staticTimeNonInherited();
		Abraham.staticTimeNonInherited();
		DiskManager.staticTimeNonInherited();
		FlockInfo.staticTimeNonInherited();
		//TODORecipe.staticTimeNonInherited();
		
		initialized = true;
	}
}
