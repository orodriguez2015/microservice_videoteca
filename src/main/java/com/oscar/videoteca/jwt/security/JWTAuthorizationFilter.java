package com.oscar.videoteca.jwt.security;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.context.support.SpringBeanAutowiringSupport;
import org.springframework.web.filter.OncePerRequestFilter;

import com.oscar.videoteca.constants.ConstantsAuthentication;
import com.oscar.videoteca.rest.authentication.jwt.JwtTokenUtil;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.MalformedJwtException;
import io.jsonwebtoken.UnsupportedJwtException;

/**
 * Filtro de Spring que comprueba que en cada petición a un operación de un servicio, se haya incluido
 * el token JWT en la cabecera de la petición
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Component
public class JWTAuthorizationFilter extends OncePerRequestFilter {
	
	@Override
	protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain) throws ServletException, IOException {
		try {
			 SpringBeanAutowiringSupport.processInjectionBasedOnCurrentContext(this);
			 
			if (existsJWTToken(request, response)) {
				Claims claims = validateToken(request);
				if (claims.get("authorities") != null) {
					setUpSpringAuthentication(claims);
				} else {
					SecurityContextHolder.clearContext();
				}
			} else {
					SecurityContextHolder.clearContext();
			}
			chain.doFilter(request, response);
		} catch (ExpiredJwtException | UnsupportedJwtException | MalformedJwtException e) {
			response.setStatus(HttpServletResponse.SC_FORBIDDEN);
			((HttpServletResponse) response).sendError(HttpServletResponse.SC_FORBIDDEN, e.getMessage());
			return;
		}
	}	

	
	/**
	 * Comprueba si en la petición, en caso de necesitar autorización, 
	 * @param request
	 * @return
	 */
	private Claims validateToken(HttpServletRequest request) {	
		JwtTokenUtil util = new JwtTokenUtil();	
		return util.validateTokenNuevo(request);

	}

	/**
	 * Metodo para autenticarnos dentro del flujo de Spring
	 * 
	 * @param claims
	 */
	private void setUpSpringAuthentication(Claims claims) {
		@SuppressWarnings("unchecked")
		List<String> authorities = (List) claims.get("authorities");

		UsernamePasswordAuthenticationToken auth = new UsernamePasswordAuthenticationToken(claims.getSubject(), null,
				authorities.stream().map(SimpleGrantedAuthority::new).collect(Collectors.toList()));
		SecurityContextHolder.getContext().setAuthentication(auth);

	}

	/**
	 * Comprueba si en la petición existe la cabecera Authrorization para recuperar el token JWT
	 * @param request HttpServletRequest
	 * @param res HttpServletResponse
	 * @return Boolean
	 */
	private Boolean existsJWTToken(HttpServletRequest request, HttpServletResponse res) {
		String authenticationHeader = request.getHeader(ConstantsAuthentication.HEADER_AUTHORIZATION);
		if (authenticationHeader == null || !authenticationHeader.startsWith(ConstantsAuthentication.BEARER_JWT_PREFFIX))
			return Boolean.FALSE;
		return Boolean.TRUE;
	}

}