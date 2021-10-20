package com.oscar.videoteca.rest.dto;

import java.io.Serializable;

import javassist.SerialVersionUID;
import lombok.Getter;
import lombok.Setter;

/**
 * Clase LoginDTO que alberga el login y password de un usuario a la hora 
 * de autenticarse
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
@Getter
@Setter
public class LoginDTO implements Serializable {
	/**
	 * {@link SerialVersionUID}
	 */
	private static final long serialVersionUID = -6400714018004551720L;
	/**
	 * Login del usuario
	 */
	private String login;
	/**
	 * Password del usuario
	 */
	private String password;

}
