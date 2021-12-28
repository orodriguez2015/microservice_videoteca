import React from 'react';
import ReactDOM from 'react-dom';
import Table from 'react-bootstrap/Table';
import ErrorMessage from '../../../error/ErrorMessage';
import {AlmacenFacade} from '../../../../store/AlmacenFacade';
import {AlbumFacade} from '../../../../facade/AlbumFacade';
import ComponenteAutenticado from '../autenticacion/ComponenteAutenticado';
import ModalConfirmation from '../../../modal/ModalConfirmation';


/**
 * Componente que genera un listado con los álbumes fotográficos de un determinado usuario
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class AlbumesPrivados extends ComponenteAutenticado {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.state = {
            albumes: [],
            error: false
        }

        this.handleAlbumSelected = this.handleAlbumSelected.bind(this);
        this.handleAdjuntarFoto = this.handleAdjuntarFoto.bind(this);    
    }

    /**
     * Método componentDidMount
     */
    componentDidMount() {

        if(this.estaUsuarioAutenticado()) {

            let user = AlmacenFacade.getUser();
            
            AlbumFacade.getAlbumesUsuario(user).then(resultado=>{
                if(resultado!==undefined && resultado!==null) {

                    if(resultado.status!==undefined) {
                        // Se ha producido un error de tipo 404,500, etc
                        this.setState({
                            albumes: [],
                            error: true,
                            descError: "Se ha producido un error al recuperar sus álbumes fotográficos"
                        });
                    } else {
                        this.setState({
                            albumes: resultado,
                            error: false,
                            descError: ''
                        });
                    }
                 }
                
            }).catch(err=>{
                this.setState({
                    albumes: [],
                    error: true,
                    descError: "Se ha producido un error al recuperar sus álbumes fotográficos"
                });
            });
        }
    }

    /**
     * Manejador de la selección de un álbum
     * @param {Integer} idAlbum 
     */
    handleAlbumSelected(idAlbum) { console.log("aqui ");
        
        if(idAlbum!==null && idAlbum!==undefined) {
           window.location.href="/pr_album/" + idAlbum;
        } 
    }

    /**
     * Manejador de la selección de un álbum para proceder a su borrado
     * @param {Integer} idAlbum 
     */
    handleEliminarAlbum(idAlbum) {
        if(idAlbum!==null && idAlbum!==undefined) {
            const mensaje = "¿Desea eliminar el álbum seleccionado con #Id " + idAlbum + " ?";
            // Se renderiza el componente WindowModalLogin indicando en la propiedad que se debe mostrar
            ReactDOM.render(<ModalConfirmation show={true} title="Atención" onConfirm={()=>this.onConfirmEliminarAlbum(idAlbum)} message={mensaje} textButtonConfirm="Aceptar" textButtonCancel="Cancelar"/>, document.getElementById('ventanaModal'));
        }
    }


    /**
     * Función que se invoca cuando el usuario ha confirmado la eliminación de un álbum
     * @param {Integer} idAlbum Id del álbum
     */
    onConfirmEliminarAlbum(idAlbum) {
        if(idAlbum!==null && idAlbum!==undefined) {
            const user = AlmacenFacade.getUser();

            AlbumFacade.deleteAlbum(idAlbum,user.id)
            .then(resultado=>{
                if(resultado.status===0) {
                    window.location.href="/pr_albumes";
                }

            }).catch(err=>{
                console.log("Se ha producido un error al borrar el álbum");
            });
        }
    }

   
    /**
     * Manejador de la edición de un álbum
     * @param {Integer} idAlbum 
     */
    handleEditarAlbum(idAlbum) {
        if(idAlbum!==null && idAlbum!==undefined) {
            window.location.href="/pr_album/edit/" + idAlbum;
        }
    }


    /**
     * Manejador para poder adjuntar fotografías 
     * @param {Integer} idAlbum 
     */
    handleAdjuntarFoto(idAlbum) {
        if(idAlbum!==null && idAlbum!==undefined) {
            window.location.href="/pr_album/adjuntar/" + idAlbum;
        }
    }


    /**
     * Método render
     */
    render() {

        if(this.hayErrores()===true) {
            return (<ErrorMessage mensaje={this.getMensajeError()}/>);   
        } else {
            return (
                <div className="container">
                    <div className="subtitulo">
                        <h2>Gestiona tus álbumes fotográficos</h2>
                    </div>
                
                    <Table responsive stripped="true" className="stripped table-hover">
                    <thead>
                        <tr>
                        <th># Id</th>
                        <th>Nombre</th>
                        <th>Descripción</th>
                        <th>Público</th>
                        <th>Fecha de alta</th>
                        <th></th>
                        </tr>
                    </thead>
                    <tbody>

                    {this.state.albumes.map((value, index) => {
                        let publico = (value.publico===true)?'SI':'NO';
                         return (
                            <tr key={value.id}>
                                <td><a className="visorMultimedia" href={'/pr_album/' + value.ID}>{value.id}</a></td>
                                <td><a className="visorMultimedia" href={'/pr_album/' + value.ID}>{value.nombre}</a></td>
                                <td><a className="visorMultimedia" href={'/pr_album/' + value.ID}>{value.descripcion}</a></td>
                                <td><a className="visorMultimedia" href={'/pr_album/' + value.ID}>{publico}</a></td>
                                <td><a className="visorMultimedia" href={'/pr_album/' + value.ID}>{value.fechaAlta}</a></td>
                                <td>
                                    <img src="/images/pencil2.png" width="22" onClick={()=>this.handleEditarAlbum(value.id)} alt="Editar álbum" title="Editar álbum"/>
                                    &nbsp;                                    
                                    <img src="/images/backup.jpeg" width="22" onClick={()=>this.handleAdjuntarFoto(value.id)} alt="Adjuntar vídeo" title="Adjuntar vídeo"/>
                                    &nbsp;
                                    <img src="/images/full_trash.png" width="22" onClick={()=>this.handleEliminarAlbum(value.id)} alt="Eliminar álbum" title="Eliminar álbum"/>
                                </td>                               
                            </tr>)                           
                    })}
                    </tbody>
                    </Table>
                </div>
            );
        }
    }
}

export default AlbumesPrivados;