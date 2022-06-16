package com.oscar.videoteca.rest.util;

import com.oscar.videoteca.rest.model.entity.Photo;

public class PhotoVisibilityFactory {

	/**
	 * Establece la visibilidad de una fotografía en función del parámetro <param>visibility</param>
	 * @param photo Photo
	 * @param visibility PhotoVisibilityEnum
	 */
	public static void establishVisibilityPhotography(Photo photo,PhotoVisibilityEnum visibility) {
		if(visibility.equals(PhotoVisibilityEnum.PUBLISHED_PHOTO)) {
			photo.setPublico(Boolean.TRUE);
		}else
		if(visibility.equals(PhotoVisibilityEnum.PRIVATE_PHOTO)) {
			photo.setPublico(Boolean.FALSE);
		}
	}
}
