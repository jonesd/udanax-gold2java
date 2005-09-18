/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package org.abora.gold.java.urdi;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedSet;

import org.abora.gold.collection.basic.UInt8Array;
import org.abora.gold.java.exception.AboraRuntimeException;
import org.abora.gold.xpp.basic.Heaper;

public class Urdi extends Heaper {
	private String filename;
	private int lruCount;

	private Map space = new HashMap();
	private int totalSnarfs = -1;
	
	private UrdiView lastView = null;
	
	private static final int DEFAULT_TOTAL_SNARFS = 32;
	
	//TODO guess at size. Seems like it needs to be a multiple of 4 in size
	protected static final int SNARF_SIZE = 4000;
	
	public Urdi(String filename, int lruCount) {
		super();
		this.filename = filename;
		this.lruCount = lruCount;
		
		try {
			File file = new File(filename);
			if (file.exists()) {
				loadFile();
			} else {
				initializeEmptySpace();
				writeFile();
			}
		} catch (IOException e) {
			throw new AboraRuntimeException("Failed to initialize Urd: "+this+" due to: "+e);
		}
	}

	private void initializeEmptySpace() {
		totalSnarfs = DEFAULT_TOTAL_SNARFS;
		for (int i = 0; i < totalSnarfs; i++) {
			UInt8Array snarfSpace = UInt8Array.make(SNARF_SIZE);
			space.put(new Integer(i), snarfSpace);
		}
	}

	private void writeFile() throws IOException {
		FileOutputStream fileOutputStream = new FileOutputStream(filename, false);
		try {
			for (int i = 0; i < totalSnarfs; i++) {
				UInt8Array blockArray = (UInt8Array)space.get(new Integer(i));
				byte[] block = blockArray.gutsOfByteArray();
				try {
					if (block.length != SNARF_SIZE) {
						throw new IllegalStateException("Snarf not expected size: "+SNARF_SIZE+", but was: "+block.length);
					}
					fileOutputStream.write(block);
				} finally {
					blockArray.noMoreGuts();
				}
			}
		} finally {
			fileOutputStream.close();
		}
	}

	private void loadFile() throws IOException {
		if (!space.isEmpty()) {
			throw new IllegalStateException("Urdi space already contains content, cant load: "+this);
		}
		FileInputStream fileInputStream = new FileInputStream(filename);
		try {
			totalSnarfs = 0;
			while (readSnarf(fileInputStream, totalSnarfs)) {
				totalSnarfs += 1;
			}
			
		} finally {
			fileInputStream.close();
		}
		
	}
	
	private boolean readSnarf(FileInputStream fileInputStream, int index) throws IOException {
		byte[] block = new byte[SNARF_SIZE];
		int next = 0;
		do {
			int read = fileInputStream.read(block, next, block.length - next);
			if (read != -1) {
				next += read;
			} else {
				next = -1;
			}
		} while (next != -1 && next != block.length);
		UInt8Array array = UInt8Array.makeShared(block);
		space.put(new Integer(index), array);
		return next != -1;
	}

	public UrdiView makeWriteView() {
		return onlyView(new UrdiView(this, true));
	}

	public static Urdi urdi(String fname, int lruCount) {
		return new Urdi(fname, lruCount);
	}

	private UrdiView onlyView(UrdiView view) {
		if (lastView != null) {
			lastView.spent = true;
		}
		lastView = view;
		return view;
	}

	public int usableSnarfs() {
		//TODO rubbish - See SnarfInfoHandle
		return totalSnarfs;
	}

	public int getDataSizeOfSnarf(int i) {
		return SNARF_SIZE;
	}

	public int usableStages() {
		//TODO have no idea what this means...
		return 100;
	}

	public UrdiView makeReadView() {
		return onlyView(new UrdiView(this, false));
	}
	
	public void printOn(PrintWriter oo) {
		oo.print(getAboraClass().name());
		oo.print("(");
		oo.print(filename);
		oo.print(")");
	}

	protected UInt8Array getSpace(int snarfID) {
		UInt8Array snarfSpace = (UInt8Array)space.get(new Integer(snarfID));
		if (snarfSpace == null) {
			throw new AboraRuntimeException("Unknown snarfId: "+snarfID+" for urdi: "+this);
		}
		return snarfSpace;
	}

	public void writeSnarfs(SortedSet toCommit) {
		System.out.print(toString()+" writeAll: (");
		for (Iterator iter = toCommit.iterator(); iter.hasNext();) {
			SnarfHandle handle = (SnarfHandle) iter.next();
			System.out.print(handle.getSnarfID());
			System.out.print(" ");
		}
		System.out.println(")");
		
		try {
			//TODO cache stream
			// Review mode - need some forcing going on here!
			RandomAccessFile file = new RandomAccessFile(filename, "rwd");
			try {
				for (Iterator iter = toCommit.iterator(); iter.hasNext();) {
					SnarfHandle handle = (SnarfHandle) iter.next();					
					UInt8Array array = handle.getData();
					file.seek(handle.getSnarfID() * (long)SNARF_SIZE);
					//TODO not safe if later failure
					try {
						file.write(array.gutsOfByteArray());
					} finally {
						array.noMoreGuts();
					}
					space.put(new Integer(handle.getSnarfID()), array);
				}
			} finally {
				file.close();
			}
			
		} catch (IOException e) {
			// TODO not transactionally safe...
			System.out.println("WARNING: Transactionally unsafe failure of writeSnarf");
			throw new AboraRuntimeException("Failed to write snarfs, due to: "+e);
		}
		
	}

}
