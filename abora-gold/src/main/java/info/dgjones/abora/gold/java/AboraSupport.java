/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.gold.java;

import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;

import info.dgjones.abora.gold.java.missing.smalltalk.AboraClass;
import info.dgjones.abora.gold.java.missing.smalltalk.OrderedCollection;
import info.dgjones.abora.gold.java.missing.smalltalk.Smalltalk;
import info.dgjones.abora.gold.xpp.basic.Category;
import info.dgjones.abora.gold.xpp.basic.Heaper;



public class AboraSupport {

	public static PrintWriter logger = null; 
	
	private static final Map categories = new HashMap();
	
	private static final Map globals = new HashMap();
	
	public AboraSupport() {
		super();
	}

	public static Category findCategory(String className) {
		Class c;
		try {
			c = Class.forName(className);
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException("Couldn't find class: "+className+" "+e);
		}
		return findCategory(c);
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
		return AboraClass.findAboraClass(c);
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
		return findAboraClass(c).allSubclasses();
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
		String result = cuisine.toLowerCase();
		if (result.length() > 0) {
			char capatal = Character.toUpperCase(result.charAt(0));
			result = String.valueOf(capatal)+result.substring(1);
		}
		return result;
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
	 * infinity, rather than the Java / operator which rounds towards 0.
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

	public static Fn pointerToStaticMember(Category category, String functionName) {
		
		// TODO Auto-generated method stub
		return null;
	}

	
	public static String toBaseString(int value, int base) {
		/* compatibility with ug implementation - lower case alphas */
		return Integer.toString(value, base).toUpperCase();
	}

	public static void defineGlobalRecipe(String recipeName, Heaper initialValue) {
		Smalltalk.atPut(recipeName, null);
		
	}
	
}
