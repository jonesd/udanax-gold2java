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
package info.dgjones.abora.gold.java.missing;

import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * Mickey mouse implementation to keep code references happy, styled after XPP sema4x.cxx.
 * 
 * The current translated Java code is actually using the Sema4 instances as monitor
 * locks.
 * 
 * TODO review the use of these instances!
 */
public class Sema4 extends Heaper {

	protected int count;
	
	public Sema4(int initialCount) {
		super();
		this.count = initialCount;
	}
	public static Sema4 make(int initialCount) {
		return new Sema4(initialCount);
	}
	
	public void v() {
		count += 1;
	}
	
	public void p() {
		count -= 1;
	}
	
	public int t() {
		int result = count;
		if (count > 0) {
			count -= 1;
		}
		return result;
	}

}
