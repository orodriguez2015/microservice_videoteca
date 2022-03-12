package com.oscar.videoteca.rest.manager.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Example;
import org.springframework.data.domain.ExampleMatcher;
import org.springframework.stereotype.Service;

import com.oscar.videoteca.rest.authentication.jwt.JwtTokenUtil;
import com.oscar.videoteca.rest.dto.LoginDTO;
import com.oscar.videoteca.rest.dto.UserDTO;
import com.oscar.videoteca.rest.dto.UserLoginDTO;
import com.oscar.videoteca.rest.dto.authentication.LoginResponseDTO;
import com.oscar.videoteca.rest.dto.mapping.UserConverter;
import com.oscar.videoteca.rest.exception.UserEmailExistsException;
import com.oscar.videoteca.rest.exception.UserLoginExistsException;
import com.oscar.videoteca.rest.exception.UserNotFoundException;
import com.oscar.videoteca.rest.manager.UserManager;
import com.oscar.videoteca.rest.model.entity.User;
import com.oscar.videoteca.rest.model.repository.UserRepository;
import com.oscar.videoteca.rest.util.PasswordUtil;

/**
 * UserManagerImpl que contiene operaciones relativas al manejo de los usuarios del sistema
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez Brea</a>
 *
 */
@Service
public class UserManagerImpl implements UserManager{

	@Autowired
	private UserRepository userRepository;
	
	@Autowired
	private UserConverter userConverter;
	
	@Autowired
	private JwtTokenUtil jwtTokenUtil;

	
	/**
	 * Recupera un determinado usuario
	 * @param id Id del usuario
	 * @return UserDTO
	 * @throws UserNotFoundException si ocurre algún error	
	 */
	public UserDTO getUser(Long id) throws UserNotFoundException {
		return userRepository.findById(id).map(user->{
			return userConverter.convertTo(user);
		}).orElseThrow(() -> new UserNotFoundException(id));
		
	}
	
	
	
	@Override
	public UserDTO saveUser(UserDTO nuevo) throws UserLoginExistsException {
		UserDTO salida = null;
		
		
		if(Boolean.TRUE.equals(this.existsUserWithLogin(nuevo.getLogin()))) {
			throw new UserLoginExistsException(nuevo.getLogin());
		}
		
		
		if(Boolean.TRUE.equals(this.existsUserWithEmail(nuevo.getEmail()))) {
			throw new UserEmailExistsException(nuevo.getEmail());
		}
				
		
		// Hay que modificar la password para almacenar en BBDD el hash SHA-1 de la clave 		
		nuevo.setPassword(PasswordUtil.getSha1(nuevo.getPassword()));
		
		// Se almacena el usuario
		User user = userRepository.save(userConverter.convertTo(nuevo));
		salida = userConverter.convertTo(user);
		
		return salida;
	}
	
	


	@Override
	public LoginResponseDTO validarUsuario(LoginDTO login) throws UserNotFoundException {
		LoginResponseDTO response = new LoginResponseDTO();
		response.setCodStatus(1);
		response.setDescStatus("Autenticación incorrecta");
		
		ExampleMatcher ignoringExampleMatcher = ExampleMatcher.matchingAll()
			      .withMatcher("login", ExampleMatcher.GenericPropertyMatchers.startsWith().ignoreCase())
			      .withMatcher("password", ExampleMatcher.GenericPropertyMatchers.startsWith().ignoreCase());
			
		// Se busca un usuario con el login y el password
		User user = new User();
		user.setLogin(login.getLogin());
		user.setPassword(PasswordUtil.getSha1(login.getPassword()));
				
		Example<User> example = Example.of(user, ignoringExampleMatcher);
		
		// Se buscan los usuarios que tengan el login y password introducidos por el usuario
		List<User> usuarios = userRepository.findAll(example);
		
		if(usuarios!=null && Boolean.FALSE.equals(usuarios.isEmpty())) {
			response.setCodStatus(0);
			response.setDescStatus("Autenticacion correcta");
			
			UserLoginDTO userDTO = userConverter.convertToUserLoginDTO(usuarios.get(0));
			userDTO.setAuthenticationToken(jwtTokenUtil.generateToken(login.getLogin()));
			response.setUser(userDTO);
			
		}		
		return response;		
	}


	/**
	 * Comprueba si existen usuarios que tenga un determinado loginOpera
	 * @param login String
	 * @return Boolean
	 */
	@Override
	public Boolean existsUserWithLogin(String login) {
		Boolean exito = Boolean.FALSE;
	
		ExampleMatcher ignoringExampleMatcher = ExampleMatcher.matchingAny()
				      .withMatcher("login", ExampleMatcher.GenericPropertyMatchers.startsWith().ignoreCase());
		
		User user = new User();
		user.setLogin(login);
		Example<User> example = Example.of(user, ignoringExampleMatcher);
		
		// Se buscan los usuarios que tengan un determinado login
		List<User> usuarios = userRepository.findAll(example);
		if(usuarios!=null && Boolean.FALSE.equals(usuarios.isEmpty())) {
			exito =Boolean.TRUE;
		}
		
		return exito;
		
	}
	
	
	/**
	 * Comprueba si existen usuarios que tenga un determinado email
	 * @param email String
	 * @return Boolean
	 */
	@Override
	public Boolean existsUserWithEmail(String email) {
		Boolean exito = Boolean.FALSE;
	
		ExampleMatcher ignoringExampleMatcher = ExampleMatcher.matchingAny()
				      .withMatcher("email", ExampleMatcher.GenericPropertyMatchers.startsWith().ignoreCase());
		
		User user = new User();
		user.setEmail(email);
		Example<User> example = Example.of(user, ignoringExampleMatcher);
		
		List<User> usuarios = userRepository.findAll(example);
		
		if(usuarios!=null && Boolean.FALSE.equals(usuarios.isEmpty())) {
			exito =Boolean.TRUE;
		}
		
		return exito;
		
	}
	
	
}