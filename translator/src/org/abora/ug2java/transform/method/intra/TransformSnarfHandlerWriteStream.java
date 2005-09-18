/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.transform.method.intra;

import java.util.List;

import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.transform.method.AbstractMethodBodyTransformation;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcher;
import org.abora.ug2java.transform.tokenmatcher.TokenMatcherFactory;



/**
 * Transformation to explicity request from SnarfHandle a writeable access to its
 * underlying data structure for a write stream.
 * <p>
 * This seems to highlight a difference in the underlying Snarfhandle data holder not
 * be able to detect a write to its contents, as readStream access the same data holder,
 * and a blanket makeWriteable to the SnarfHandle causes an exception in SnarfPacker.getInitialFlock().
 */
public class TransformSnarfHandlerWriteStream extends AbstractMethodBodyTransformation {

	public TransformSnarfHandlerWriteStream() {
		super();
	}
	public TransformSnarfHandlerWriteStream(TokenMatcherFactory factory) {
		super(factory);
	}

	protected TokenMatcher matchers(TokenMatcherFactory factory) {
		return factory.seq(
				factory.token(JavaCallStart.class, "getDataP"),
				factory.token(JavaCallEnd.class));
	}

	protected int transform(JavaMethod javaMethod, List tokens, int i) {
		if (!javaMethod.getQualifiedName().equals("SnarfHandler.writeStream")) {
			return i;
		}
		JavaCallStart start = (JavaCallStart)tokens.get(i);
		start.value = "getData";
		
		return i-1;
	}
}
