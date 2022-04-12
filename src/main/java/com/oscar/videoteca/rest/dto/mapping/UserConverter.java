package com.oscar.videoteca.rest.dto.mapping;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.oscar.videoteca.rest.dto.UserDTO;
import com.oscar.videoteca.rest.dto.UserLoginDTO;
import com.oscar.videoteca.rest.model.entity.User;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class UserConverter {
	
	@Autowired
	private ModelMapper modelMapper;

	
	/**
	 * Convierte un objeto de tipo User en un UserDTO
	 * @param user User
	 * @return UserDTO
	 */
	public UserDTO convertTo(User user) {
		return modelMapper.map(user,UserDTO.class);
	}
	
	
	/**
	 * Convierte un objeto de tipo UserDTO en uno de tipo User
	 * @param user UserDTO
	 * @return User
	 */
	public User convertTo(UserDTO user) {
		return modelMapper.map(user,User.class);
	}
	
	
	/**
	 * Convierte un objeto de tipo User en uno de tipo UserLoginDTO
	 * @param user User
	 * @return UserLoginDTO
	 */
	public UserLoginDTO convertToUserLoginDTO(User user) {
		return modelMapper.map(user,UserLoginDTO.class);
	}
	
}
