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
package info.dgjones.abora.ug2java.transform.method.intra;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import info.dgjones.abora.ug2java.JavaMethod;
import info.dgjones.abora.ug2java.javatoken.JavaCallEnd;
import info.dgjones.abora.ug2java.javatoken.JavaCallStart;
import info.dgjones.abora.ug2java.javatoken.JavaCast;
import info.dgjones.abora.ug2java.javatoken.JavaIdentifier;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisEnd;
import info.dgjones.abora.ug2java.javatoken.JavaParenthesisStart;
import info.dgjones.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import info.dgjones.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



public class TransformFluidAccess extends AbstractMethodBodyTransformation {

	private static final Map FLUID_MAPPINGS;
	static {
		Map map = new HashMap();
		map.put("ActiveClubs", "MuSet"); //GUESS
		map.put("CurrentAuthor", "ID");
		map.put("CurrentBertCanopyCache", "CanopyCache");
		map.put("CurrentBertCrum", "BertCrum");
		map.put("CurrentMainReceiver", "Rcvr");
		map.put("CurrentPacker", "DiskManager");
		map.put("CurrentGrandMap", "BeGrandMap");
		map.put("CurrentKeyMaster", "FeKeyMaster");
		map.put("CurrentSensorCanopyCache", "CanopyCache");
		map.put("CurrentServer", "FeServer");
		map.put("CurrentServerConnection", "Connection");
		map.put("CurrentServerLoop", "ServerLoop");
		map.put("CurrentSession", "FeSession");
		map.put("CurrentSessions", "FePromiseSession");
		map.put("CurrentTrace", "TracePosition");
		map.put("GrandConnection", "Connection");
		map.put("InitialOwner", "ID");
		map.put("InitialEditClub", "ID");
		map.put("InitialReadClub", "ID");
		map.put("InitialSponsor", "ID");
		map.put("InsideTransactionFlag", "Boolean");
		map.put("InsideAgenda", "Boolean");
		map.put("MainActiveThunk", "Heaper");
		FLUID_MAPPINGS = Collections.unmodifiableMap(map);
	}
	

	public TransformFluidAccess() {
		super();
	}
	public TransformFluidAccess(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaIdentifier.class),
				factory.any(
						factory.token(JavaCallStart.class, "fluidGet"),
						factory.token(JavaCallStart.class, "fluidFetch")
						),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		JavaIdentifier identifier = (JavaIdentifier)tokens.get(i);
		if (FLUID_MAPPINGS.containsKey(identifier.value)) {
			String type = (String)FLUID_MAPPINGS.get(identifier.value);
			tokens.add(i + 2, new JavaParenthesisEnd());
			tokens.add(i, new JavaParenthesisStart());
			tokens.add(i + 1, new JavaCast(type));
			if (type.equals("Boolean")) {
				tokens.add(i+6, new JavaCallStart("booleanValue"));
				tokens.add(i+7, new JavaCallEnd());
			}
		}
		return i;
	}
}
