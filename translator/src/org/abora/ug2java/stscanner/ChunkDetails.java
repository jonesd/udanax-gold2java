/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.stscanner;

public class ChunkDetails {
	public String context;
	public String contents;

	public ChunkDetails() {
		super();
	}

	public ChunkDetails(String filename, int lineNumber, String description, String contents) {
		this(filename + ":" + lineNumber + ":" + description, contents);
	}

	public ChunkDetails(String context, String contents) {
		super();
		this.context = context;
		this.contents = contents;
	}
}
