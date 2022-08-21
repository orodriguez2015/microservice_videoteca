package com.oscar.videoteca.rest.util;

import com.oscar.videoteca.rest.model.entity.Photo;

public class PhotoVisibilityFactory {

	/**
	 * Establece la visibilidad de una fotografía en función del parámetro <param>visibility</param>
	 * @param photo Photo
	 * @param visibility PhotoVisibilityEnum
	 */
	public static void establishVisibilityPhotography(Photo photo,ResourceVisibilityEnum visibility) {
		if(visibility.equals(ResourceVisibilityEnum.PUBLISHED)) {
			photo.setPublico(Boolean.TRUE);
		}else
		if(visibility.equals(ResourceVisibilityEnum.PRIVATE)) {
			photo.setPublico(Boolean.FALSE);
		}
	}
}
