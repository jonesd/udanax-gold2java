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



public class HashHelper {

	public static int hashForEqual(Class clazz) {
		//TODO guess!
		return hashForEqual(clazz.getName());
	}
	
	public static int hashForEqual(int value) {
		//TODO guess!
		return Math.abs(value);
	}

	public static int hashForEqual(double d) {
		//TODO guess!
		return (int)Math.abs(d);
	}

	public static int hashForEqual(float f) {
		//TODO guess!
		return (int)Math.abs(f);
	}

	public static int hashForEqual(String s) {
		//TODO bad!! Probably shouldn't rely on Java hashCode behaviour
		return s.hashCode();
	}

}
