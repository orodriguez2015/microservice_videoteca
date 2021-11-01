package com.oscar.videoteca.constants;

/**
 * Clase con constantes relacionadas con la autenticacion
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
public class ConstantsAuthentication {

	// Spring Security
	public static final String LOGIN_URL = "/login";
	public static final String HEADER_AUTHORIZACION_KEY = "Authorization";
	public static final String TOKEN_BEARER_PREFIX = "Bearer ";

	// JWT
	public static final String HEADER_AUTHORIZATION = "Authorization";
	public static final String BEARER_JWT_PREFFIX 	= "Bearer ";
	public static final String SECRET_KEY     		= "enunlugardelamancha";
	public static final String ID_JWT  		 		= "videotecaJWT";
	
		
}