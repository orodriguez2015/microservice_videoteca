package com.oscar.videoteca.rest.authentication.jwt;

import java.io.Serializable;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.stereotype.Component;

import com.oscar.videoteca.constants.ConstantsAuthentication;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;


/**
 * Clase JWTUtil con operaciones de utilidad para la autenticación JWT (JSON WEB TOKEN)
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Component
public class JwtTokenUtil implements Serializable {

	private static final long serialVersionUID = -2550185165626007488L;
	public static final long JWT_TOKEN_VALIDITY = 5 * 60 * 60;	

	/**
	 * Devuelve un token JWT para un determinado usuario
	 * @param username Login del usuario
	 * @return Token
	 */
	public String generateToken(String username) {
		return doGenerateTokenNuevo(username);
	}

	
	/**
	 * Genera un token JWT para un usuario
	 * @param username String
	 * @return String con el token
	 */
	private String doGenerateTokenNuevo(String username) {
		List<GrantedAuthority> grantedAuthorities = AuthorityUtils
				.commaSeparatedStringToAuthorityList("ROLE_USER");
		
		String token = Jwts
				.builder()
				.setId(ConstantsAuthentication.ID_JWT)
				.setSubject(username)
				.claim("authorities",
						grantedAuthorities.stream()
								.map(GrantedAuthority::getAuthority)
								.collect(Collectors.toList()))
				.setIssuedAt(new Date(System.currentTimeMillis()))
				.setExpiration(new Date(System.currentTimeMillis() + 600000))
				.signWith(SignatureAlgorithm.HS512,
						ConstantsAuthentication.SECRET_KEY.getBytes()).compact();

		return ConstantsAuthentication.BEARER_JWT_PREFFIX + token;	
	}


	/**
	 * Recupera de la petición a un servicio, recuperando de la petición la cabecera Authentication, y validando el token JWT
	 * @param request HttpServletRequest
	 * @return Claims
	 */
	public Claims validateTokenNuevo(HttpServletRequest request) {
		String jwtToken = request.getHeader(ConstantsAuthentication.HEADER_AUTHORIZATION).replace(ConstantsAuthentication.BEARER_JWT_PREFFIX, "");
		
		Claims claims =  Jwts.parser().setSigningKey(ConstantsAuthentication.SECRET_KEY.getBytes()).parseClaimsJws(jwtToken).getBody();		
		return claims;
	}	
		
}