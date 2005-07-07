package org.abora.gold.java;

import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;

import org.abora.gold.java.missing.smalltalk.AboraClass;
import org.abora.gold.java.missing.smalltalk.OrderedCollection;
import org.abora.gold.xpp.basic.Category;



public class AboraSupport {

	public static PrintWriter logger = null; 
	
	private static final Map categories = new HashMap();
	private static final Map aboraClasses = new HashMap();
	
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
		throw new UnsupportedOperationException();
	}

	public static int xuTime() {
		throw new UnsupportedOperationException();
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
	
}
