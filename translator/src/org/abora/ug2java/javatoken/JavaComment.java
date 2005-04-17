/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.javatoken;

import org.abora.ug2java.writer.JavaWriter;

public class JavaComment extends JavaToken {

	public JavaComment(String value) {
		super(value);
	}

	public void write(JavaWriter buffer) {
		buffer.ensureAtStartOfLine();
		if (value.indexOf("/*") == -1) {
			buffer.append("/* ");
			super.write(buffer);
			buffer.append(" */");
		} else {
			String clean = value.replaceAll("\n", "\n// ");
			buffer.append("// ");
			buffer.append(clean);
		}
		buffer.newLine();
	}
}