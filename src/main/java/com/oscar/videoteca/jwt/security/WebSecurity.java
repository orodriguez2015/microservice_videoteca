package com.oscar.videoteca.jwt.security;

import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;



/**
 * Configuración de seguridad
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Configuration
@EnableWebSecurity
public class WebSecurity extends WebSecurityConfigurerAdapter {	
	
	 private final String[] AUTH_WHITELIST = {
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
	            "/swagger-ui/**"
	            // other public endpoints of your API may be appended to this array
	    };
	
	@Override
	protected void configure(HttpSecurity http) throws Exception {
		http.csrf().disable()
			.addFilterAfter(new JWTAuthorizationFilter(), UsernamePasswordAuthenticationFilter.class)
			.authorizeRequests()
			.antMatchers(HttpMethod.POST, "/login").permitAll()
			.antMatchers(HttpMethod.GET, "/p_videoteca").permitAll()
			//.antMatchers(HttpMethod.GET,"/swagger-ui.html").permitAll()
			.antMatchers(HttpMethod.GET,AUTH_WHITELIST).permitAll()
			.anyRequest().authenticated();
	}
	

}