package com.oscar.videoteca.rest.util;

import java.text.SimpleDateFormat;
import java.util.Calendar;

/**
 * Clase DateOperations
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
public class DateOperations {

	public static String DD_MM_YYYY_hh_mm_ss = "dd-MM-yyyy-hh-mm-ss";
	
	/**
	 * Constructor
	 */
	private DateOperations() {
		
	}
	
	/**
	 * Devuelve la fecha actual en el formato que se pase por parámetro
	 * @param format String
	 * @return Strig
	 */
	public String getTimestamp(String format) {
		SimpleDateFormat sf = new SimpleDateFormat(format);
		return sf.format(Calendar.getInstance().getTime());
	}
	
}
