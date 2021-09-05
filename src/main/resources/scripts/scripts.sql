CREATE TABLE videoteca.`user` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `login` varchar(255) DEFAULT NULL,
  `password` varchar(255) DEFAULT NULL,
  `nombre` varchar(255) DEFAULT NULL,
  `apellido1` varchar(255) DEFAULT NULL,
  `apellido2` varchar(255) DEFAULT NULL,
  `email` varchar(255) DEFAULT NULL,
  `fecha_alta` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `fecha_modificacion` datetime DEFAULT NULL,
  `activo` int(1) NOT NULL DEFAULT '1',
  `fecha_baja` datetime DEFAULT NULL,
  `root` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB;



INSERT INTO videoteca.user(LOGIN,PASSWORD,NOMBRE,APELLIDO1,APELLIDO2,EMAIL,ACTIVO,ROOT) VALUES('admin','d033e22ae348aeb5660fc2140aec35850c4da997','Administrador','','','admin@root.com',1,1)


CREATE TABLE videoteca.`album` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `nombre` varchar(255) DEFAULT NULL,
  `fecha_alta` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `fecha_modificacion` datetime DEFAULT NULL,
  `id_usuario` int(11) NOT NULL,
  `publico` int(11) NOT NULL,
  `descripcion` varchar(225) NOT NULL,
  `id_usuario_mod` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `albums_ibfk_2` (`id_usuario_mod`),
  KEY `albums_ibfk_1` (`id_usuario`),
  CONSTRAINT `albums_ibfk_1` FOREIGN KEY (`id_usuario`) REFERENCES `user` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `albums_ibfk_2` FOREIGN KEY (`id_usuario_mod`) REFERENCES `user` (`id`) ON DELETE SET NULL ON UPDATE SET NULL
) ENGINE=InnoDB DEFAULT CHARSET=UTF8;


CREATE TABLE videoteca.`foto` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `nombre` varchar(45) NOT NULL,
  `ruta` varchar(225) NOT NULL,
  `rutaMiniatura` varchar(225) NOT NULL,
  `alto` int(11) NOT NULL,
  `ancho` int(11) NOT NULL,
  `tipomime` varchar(45) DEFAULT NULL,
  `id_album` int(11) NOT NULL COMMENT 'Clave foránea que indica el álbum al que pertenece la fotografía',
  `fecha_alta` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `id_usuario` int(11) DEFAULT NULL,
  `publico` int(11) NOT NULL DEFAULT '1',
  `numero_visualizaciones` int(11) DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `idAlbumFk1` (`id_album`),
  KEY `fkIdUsuario_idx` (`id_usuario`),
  CONSTRAINT `fkIdUsuario` FOREIGN KEY (`id_usuario`) REFERENCES `user` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fkidAlbum` FOREIGN KEY (`id_album`) REFERENCES `album` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=UTF8;



CREATE TABLE videoteca.`videoteca` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `nombre` varchar(255) NOT NULL,
  `ruta` varchar(125) NOT NULL,
  `ruta_completa` varchar(255) NOT NULL,
  `id_usuario` int(11) NOT NULL,
  `fecha_alta` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `fecha_modificacion` datetime DEFAULT NULL,
  `fecha_baja` datetime DEFAULT NULL,
  `publico` int(11) NOT NULL,
  `ruta_carpeta_relativa` varchar(255) not null,
  PRIMARY KEY(id),
  KEY `video_ibfk_1` (`id_usuario`),
  CONSTRAINT `video_ibfk_1` FOREIGN KEY (`id_usuario`) REFERENCES `user` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=UTF8;





CREATE TABLE videoteca.`video` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `nombre` varchar(225) NOT NULL,
  `extension` varchar(15) NOT NULL,
  `publico` int(1) NOT NULL,
  `id_videoteca` int(11) NOT NULL,
  `id_usuario` int(11) NOT NULL,
  `fecha_alta` datetime NOT NULL,
  PRIMARY KEY (`id`),
  KEY `id_usuario_idx` (`id_usuario`),
  KEY `id_videoteca_idx` (`id_videoteca`),
  CONSTRAINT `id_usuario` FOREIGN KEY (`id_usuario`) REFERENCES `user` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `id_videoteca` FOREIGN KEY (`id_videoteca`) REFERENCES `videoteca` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=UTF8;




CREATE TABLE videoteca.`permisos` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `permiso` varchar(125) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=UTF8;


CREATE TABLE videoteca.`permisos_usuario` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `id_usuario` int(11) NOT NULL,
  `id_permiso` int(11) NOT NULL,
  `fecha_alta` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  KEY `fkIdPUsuario_idx` (`id_usuario`),
  KEY `fkIdPermiso_idx` (`id_permiso`),
  CONSTRAINT `fkIdPUsuario` FOREIGN KEY (`id_usuario`) REFERENCES `user` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fkIdPermiso`  FOREIGN KEY (`id_permiso`) REFERENCES `permisos` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB;


INSERT INTO `videoteca`.`permisos` (`permiso`) VALUES ('Alta de álbum');

INSERT INTO `videoteca`.`permisos` (`permiso`) VALUES ('Alta de usuario');

INSERT INTO `videoteca`.`permisos` (`permiso`) VALUES ('Baja de álbum');

INSERT INTO `videoteca`.`permisos` (`permiso`) VALUES ('Baja de usuario');

INSERT INTO `videoteca`.`permisos` (`permiso`) VALUES ('Edición de álbum');

INSERT INTO `videoteca`.`permisos` (`permiso`) VALUES ('Edición de usuario');
















