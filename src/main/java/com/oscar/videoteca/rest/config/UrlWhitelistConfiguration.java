package com.oscar.videoteca.rest.config;

import java.util.List;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import lombok.Setter;

import lombok.Getter;

/**
 * Clase que accede al fichero de configuración application.yml para recuperar la 
 * configuración de lista de url de servicios a los que se puede acceder sin necesidad de utilizar 
 * la autenticación JWT.
 * 
 * Se tiene en cuenta que hay url a las que se accede por GET y otras por POST
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Component
@Getter
@Setter
@ConfigurationProperties(prefix = "authentication")
public class UrlWhitelistConfiguration {

	private List<String> urlWhiteListByGet;
	
	private List<String> urlWhiteListByPost;
	
	/**
	 * Devuelve un String[] con la lista blanca de urlśm de servicios a los que se accede por GET
	 * y que no requieren autenticación
	 * @return
	 */
	public String[] getArrayListaBlancaUrlPorGet() {
		String[] salida = null;
	
		if(urlWhiteListByGet!=null) {		
			salida = this.urlWhiteListByGet.toArray(new String[urlWhiteListByGet.size()]);
		}
		return salida;
	}
	
	
	/**
	 * Devuelve un String[] con la lista blanca de urlśm de servicios a los que se accede por GET
	 * y que no requieren autenticación
	 * @return
	 */
	public String[] getArrayListaBlancaUrlPorPost() {
		String[] salida = null;
	
		if(urlWhiteListByPost!=null) {
			salida = this.urlWhiteListByPost.toArray(new String[urlWhiteListByPost.size()]);
		}
		return salida;
	}

}
