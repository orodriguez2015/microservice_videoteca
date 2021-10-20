package com.oscar.videoteca.rest.manager;

import com.oscar.videoteca.rest.dto.LoginDTO;
import com.oscar.videoteca.rest.dto.UserDTO;
import com.oscar.videoteca.rest.dto.authentication.OperationResponseDTO;
import com.oscar.videoteca.rest.exception.UserLoginExistsException;
import com.oscar.videoteca.rest.exception.UserNotFoundException;

/**
 * Interface UserManager
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez Brea</a>
 *
 */
public interface UserManager {

	/**
	 * Permite dar de alta un nuevo usuario
	 * @param nuevo UserDTO
	 * @return
	 * @throws UserLoginExistsException
	 */
	UserDTO saveUser(UserDTO nuevo) throws UserLoginExistsException;
	
	
	/**
	 * Comprueba si existen usuarios que tenga un determinado login
	 * @param login String
	 * @return Boolean
	 */
	Boolean existsUserWithLogin(String login);
	
	
	/**
	 * Comprueba si existen usuarios que tenga un determinado email
	 * @param email String
	 * @return Boolean
	 */
	Boolean existsUserWithEmail(String email);
	
	
	/**
	 * Recupera un determinado usuario
	 * @param id Id del usuario
	 * @return UserDTO
	 * @throws UserNotFoundException si ocurre algún error
	 */
	UserDTO getUser(Long id) throws UserNotFoundException;
	
	
	/**
	 * Valida un usuario contra la BBDD
	 * @param login
	 * @return
	 * @throws OperationResponseDTO
	 */
	OperationResponseDTO validarUsuario(LoginDTO login) throws UserNotFoundException;
}
