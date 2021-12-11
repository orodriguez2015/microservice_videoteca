package com.oscar.videoteca.jwt.security;

import org.springframework.beans.factory.annotation.Autowired;
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

import com.oscar.videoteca.rest.config.ConfigurationListaBlancaUrl;


/**
 * Clase WebSecurity dedicada a la configuración de seguridad.
 * Configura cors y la url's de los servicios a los que se puede
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Configuration
@EnableWebSecurity
public class WebSecurity extends WebSecurityConfigurerAdapter {	
	
	@Autowired
	private ConfigurationListaBlancaUrl urlWhiteList;
	
	 
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
			.antMatchers(HttpMethod.GET,urlWhiteList.getArrayListaBlancaUrlPorGet()).permitAll() // Lista blanca de url permitidas por GET que no necesitan autenticación JWT
			.antMatchers(HttpMethod.POST,urlWhiteList.getArrayListaBlancaUrlPorPost()).permitAll() // Lista blanca de url permitidas por POST que no necesitan autenticación JWT
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