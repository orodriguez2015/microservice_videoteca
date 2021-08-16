CREATE TABLE videoteca.`Users` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `login` varchar(255) DEFAULT NULL,
  `password` varchar(255) DEFAULT NULL,
  `nombre` varchar(255) DEFAULT NULL,
  `apellido1` varchar(255) DEFAULT NULL,
  `apellido2` varchar(255) DEFAULT NULL,
  `email` varchar(255) DEFAULT NULL,
  `fechaAlta` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `fechaModificacion` datetime DEFAULT NULL,
  `activo` int(1) NOT NULL DEFAULT '1',
  `fechaBaja` datetime DEFAULT NULL,
  `root` int(11) DEFAULT '0',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB;



INSERT INTO Users(LOGIN,PASSWORD,NOMBRE,APELLIDO1,APELLIDO2,EMAIL,ACTIVO,ROOT) VALUES('admin','d033e22ae348aeb5660fc2140aec35850c4da997','Administrador','','','admin@root.com',1,1)


CREATE TABLE videoteca.`albums` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `nombre` varchar(255) DEFAULT NULL,
  `fechaAlta` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `fechaModificacion` datetime DEFAULT NULL,
  `idUsuario` int(11) NOT NULL,
  `publico` int(11) NOT NULL,
  `descripcion` varchar(225) NOT NULL,
  `idUsuarioMod` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `albums_ibfk_2` (`idUsuarioMod`),
  KEY `albums_ibfk_1` (`idUsuario`),
  CONSTRAINT `albums_ibfk_1` FOREIGN KEY (`idUsuario`) REFERENCES `Users` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `albums_ibfk_2` FOREIGN KEY (`idUsuarioMod`) REFERENCES `Users` (`id`) ON DELETE SET NULL ON UPDATE SET NULL
) ENGINE=InnoDB DEFAULT CHARSET=UTF8;


CREATE TABLE videoteca.`foto` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `nombre` varchar(45) NOT NULL,
  `ruta` varchar(225) NOT NULL,
  `rutaMiniatura` varchar(225) NOT NULL,
  `alto` int(11) NOT NULL,
  `ancho` int(11) NOT NULL,
  `tipomime` varchar(45) DEFAULT NULL,
  `idAlbum` int(11) NOT NULL COMMENT 'Clave foránea que indica el álbum al que pertenece la fotografía',
  `fechaAlta` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `idUsuario` int(11) DEFAULT NULL,
  `publico` int(11) NOT NULL DEFAULT '1',
  `numeroVisualizaciones` int(11) DEFAULT '0',
  PRIMARY KEY (`id`),
  KEY `idAlbumFk1` (`idAlbum`),
  KEY `fkIdUsuario_idx` (`idUsuario`),
  CONSTRAINT `fkIdUsuario` FOREIGN KEY (`idUsuario`) REFERENCES `users` (`id`) ON DELETE NO ACTION ON UPDATE NO ACTION,
  CONSTRAINT `fkidAlbum` FOREIGN KEY (`idAlbum`) REFERENCES `albums` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=UTF8;



CREATE TABLE videoteca.`videoteca` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `nombre` varchar(255) NOT NULL,
  `ruta` varchar(125) NOT NULL,
  `ruta_completa` varchar(255) NOT NULL,
  `idUsuario` int(11) NOT NULL,
  `fechaAlta` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `fechaModificacion` datetime DEFAULT NULL,
  `fechaBaja` datetime DEFAULT NULL,
  `publico` int(11) NOT NULL,
  `ruta_carpeta_relativa` varchar(255) not null,
  PRIMARY KEY(id),
  KEY `video_ibfk_1` (`idUsuario`),
  CONSTRAINT `video_ibfk_1` FOREIGN KEY (`idUsuario`) REFERENCES `users` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=UTF8;



