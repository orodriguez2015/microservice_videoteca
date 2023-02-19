package com.oscar.videoteca.rest.util;

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.imageio.ImageIO;

import org.imgscalr.Scalr;
import org.springframework.web.multipart.MultipartFile;

/**
 * Clase ImageUtil
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 *
 */
public class ImageUtil {

	private ImageUtil() {
		
	}
	
	/**
	 * Crea una imagen miniatura a partir de una dada
	 * @param orginalFile MultipartFile 
	 * @param width Ancho de la miniatura
	 * @return ByteArrayOutputStream
	 * @throws IOException si ocurre algún error
	 */
	public static ByteArrayOutputStream createThumbnail(MultipartFile orginalFile, Integer width) throws IOException{  
	    ByteArrayOutputStream thumbOutput = new ByteArrayOutputStream();  
	    BufferedImage thumbImg = null;  
	    BufferedImage img = ImageIO.read(orginalFile.getInputStream());  
	    thumbImg = Scalr.resize(img, Scalr.Method.AUTOMATIC, Scalr.Mode.AUTOMATIC, width, Scalr.OP_ANTIALIAS);  
	    ImageIO.write(thumbImg, orginalFile.getContentType().split("/")[1] , thumbOutput);  
	    return thumbOutput;  
	}
}
