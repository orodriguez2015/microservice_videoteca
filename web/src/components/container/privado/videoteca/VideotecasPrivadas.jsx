import React from 'react';
import ReactDOM from 'react-dom';
import Table from 'react-bootstrap/Table';
import ErrorMessage from '../../../error/ErrorMessage';
import {AlmacenFacade} from '../../../../store/AlmacenFacade';
import {VideotecasFacade} from '../../../../facade/VideotecasFacade';
import ComponenteAutenticado from '../autenticacion/ComponenteAutenticado';
import ModalConfirmation from '../../../modal/ModalConfirmation';
import {HTTP_OK} from '../../../../constantes/HttpResponse';


/**
 * Componente que genera un listado con las videotecas de un determinado usuario
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class VideotecasPrivadas extends ComponenteAutenticado {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.state = {
            videotecas: [],
            error: false
        }
        this.handleEliminarVideoteca = this.handleEliminarVideoteca.bind(this);
    }

    
    /**
     * Método componentDidMount
     */
    componentDidMount() {
        let user = AlmacenFacade.getUser();

        if(user!==undefined) {        
            VideotecasFacade.getVideosUsuario(user.id).then(resultado=>{
                if(resultado!==undefined && resultado!==null) {

                    if(resultado.codStatus===HTTP_OK) {
                        this.setState({
                            videotecas: resultado.data,
                            error: false,
                            descError: ''
                        });
                    } else {
                        this.setState({
                            albumes: [],
                            error: true,
                            descError: "Se ha producido un error al recuperar sus videotecas"
                        });
                    

                    }
                }
                
            }).catch(err=>{
                this.setState({
                    albumes: [],
                    error: true,
                    descError: "Se ha producido un error al recuperar sus videotecas"
                });
            });
        }
    }

  
    /**
     * Manejador de la selección de una videoteca para proceder a su borrado
     * @param {Integer} idVideoteca a eliminar
     */
    handleEliminarVideoteca(idVideoteca) {

        if(idVideoteca!==null && idVideoteca!==undefined) {
            const mensaje = "¿Desea eliminar la videoteca seleccionado con #Id " + idVideoteca + " ?";
            // Se renderiza el componente WindowModalLogin indicando en la propiedad que se debe mostrar
            ReactDOM.render(<ModalConfirmation show={true} title="Atención" onConfirm={()=>this.onConfirmEliminarVideoteca(idVideoteca)} message={mensaje} textButtonConfirm="Aceptar" textButtonCancel="Cancelar"/>, document.getElementById('ventanaModal'));
        }
    }

    /**
     * Función invocada cuando se ha seleccionado cuando el usuario ha decidido eliminar una videoteca
     * @param {Integer} idVideoteca 
     */
    onConfirmEliminarVideoteca(idVideoteca) {

        if(idVideoteca!==null && idVideoteca!==undefined) {
            const user = AlmacenFacade.getUser();
                    
            VideotecasFacade.deleteVideoteca(idVideoteca,user.id)
            .then(resultado=>{
                if(resultado.status===0) {
                    window.location.href="/pr_videotecas";
                }

            }).catch(err=>{
                console.log("Se ha producido un error al borrar la videoteca");
            });
        }
    }


    /**
     * Manejador de la edición de un videoteca
     * @param {Integer} idVideoteca
     */
    handleEditarVideoteca(idVideoteca) {
        if(idVideoteca!==null && idVideoteca!==undefined) {
            window.location.href="/pr_videoteca/edit/" + idVideoteca;
        }
    }



    /**
     * Manejador para la acción de adjuntar un vídeo
     * @param {Integer} idVideoteca
     */
    handleAdjuntarVideo(idVideoteca) {
        if(idVideoteca!==null && idVideoteca!==undefined) {
            window.location.href="/pr_video_adjuntar/" + idVideoteca;
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
                        <h2>Gestiona tus videotecas</h2>
                    </div>
                
                    <Table responsive stripped="true" className="stripped table-hover">
                    <thead>
                        <tr>
                        <th># Id</th>
                        <th>Nombre</th>
                        <th>Público</th>
                        <th>Fecha de alta</th>
                        <th></th>
                        </tr>
                    </thead>
                    <tbody>

                    {this.state.videotecas.map((value, index) => {
                        let publico = (value.publico===true)?'SI':'NO';
                         return (
                            <tr key={value.id}>
                                <td><a className="visorMultimedia" href={"/pr_videoteca/" + value.id}>{value.id}</a></td>
                                <td><a className="visorMultimedia" href={"/pr_videoteca/" + value.id}>{value.nombre}</a> </td>
                                <td><a className="visorMultimedia" href={"/pr_videoteca/" + value.id}>{publico}</a></td>
                                <td><a className="visorMultimedia" href={"/pr_videoteca/" + value.id}>{value.fechaAlta}</a></td>
                                <td>                                    
                                    <img src="/images/pencil2.png" width="22" onClick={()=>this.handleEditarVideoteca(value.id)} alt="Editar videoteca" title="Editar videoteca"/>
                                    &nbsp;
                                    <img src="/images/backup.jpeg" width="22" onClick={()=>this.handleAdjuntarVideo(value.id)} alt="Adjuntar vídeo" title="Adjuntar vídeo"/>
                                    &nbsp;
                                    <img src="/images/full_trash.png" width="22" onClick={()=>this.handleEliminarVideoteca(value.id)} alt="Eliminar videoteca" title="Eliminar videoteca"/>
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

export default VideotecasPrivadas;