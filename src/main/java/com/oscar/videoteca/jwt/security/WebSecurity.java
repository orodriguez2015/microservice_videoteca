package com.oscar.videoteca.jwt.security;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import com.oscar.videoteca.jwt.security.JWTAuthorizationFilter;



/**
 * Configuración de seguridad
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Configuration
@EnableWebSecurity
public class WebSecurity extends WebSecurityConfigurerAdapter {	
	
	/**
	 * Lista blanca de urlś de los microservicios que se pueden invocar por GET 
	 */
	 private final String[] AUTH_WHITELIST_GET = {			
	            // -- Swagger UI v2
	            "/v2/api-docs",
	            "/swagger-resources",
	            "/swagger-resources/**",
	            "/configuration/ui",
	            "/configuration/security",
	            "/swagger-ui.html",
	            "/webjars/**",
	            // -- Swagger UI v3 (OpenAPI)
	            "/v3/api-docs/**",
	            "/swagger-ui/**",
	            // other public endpoints of your API may be appended to this array
	         	"/login",
			 	"/p_videotecas",
			 	"/p_albumes"
	  };
	
	
	 /**
	  * Lista blanca de url's por POST
	  */
	 private final String[] AUTH_WHITELIST_POST = {			
	         	"/login"
	    };
	
	
	 
	 /**
	  * Método para configurar HttpSecurity.
	  * Se configura la autenticación por medio de token JWT
	  */
	@Override
	protected void configure(HttpSecurity http) throws Exception {
		// Configuración de HttpSecuriry para utilizar autenticación basada en un token JWT
		http.
			csrf().disable()
			.addFilterAfter(new JWTAuthorizationFilter(), UsernamePasswordAuthenticationFilter.class)
			.authorizeRequests()
			.antMatchers(HttpMethod.GET,AUTH_WHITELIST_GET).permitAll() // Lista blanca de url permitidas por GET que no necesitan autenticación JWT
			.antMatchers(HttpMethod.POST,AUTH_WHITELIST_POST).permitAll() // Lista blanca de url permitidas por POST que no necesitan autenticación JWT
			.anyRequest().authenticated();
		
		// Se habilita cors con la configuración por defecto
		// indicada en el bean CorsConfigurationSource devuelto por el 
		// método corsConfigurationSource
		http.cors().configurationSource(corsConfigurationSource());
	}
	
	
	/**
	 * Método corsConfigurationSource que habilita desde que dominio/s se pueden realizar peticiones
	 * al microservicio
	 * @return CorsConfigurationSource
	 */
	@Bean
	CorsConfigurationSource corsConfigurationSource() {
	    UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
	    source.registerCorsConfiguration("/**", new CorsConfiguration().applyPermitDefaultValues());
	    return source;
	   }

}