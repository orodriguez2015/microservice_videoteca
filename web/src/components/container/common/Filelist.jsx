import React, {Component} from 'react';
import FileUtil from '../../../util/FileUtil';

class FileList extends Component {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.areaMensajes = React.createRef;
    }

    /**
     * Método render
     */
    render() {
        // Se conprueba si hay que mostrar o no el componente
        const ficheros = this.props.ficheros;
        const mostrar = (this.props.mostrar!==undefined && this.props.mostrar===true)?true:false;                      
        const formato = this.props.verificacionFormato;
        const areaMensajesStyle = {
            display: (mostrar===true)?"block":"none"
        }
        var resultado = [];

        // Se recorre la lista de ficheros y se genera un array de objetos. Cada objeto contiene el nombre, tamaño e imagen a mostrar
        // de cada fichero
        var aux = Array.from(ficheros);
        var contador = 0;

        aux.forEach((fichero,index) =>{

            // Se comprueba si el tipo mime es correcto
            const correcto = FileUtil.verificarTipoMime(formato,fichero);
            if(correcto===false){
                contador++;
            }

            resultado.push({
                nombre: fichero.name,
                tamano: FileUtil.formatSizeUnits(fichero.size),
                imagen: (correcto===true)?'/images/correcto.png':'/images/incorrecto.png',
                txtImagen: (correcto===true)?'Formato archivo correcto':'Formato archivo incorrecto'
            });
        });
    
        if(typeof(this.props.accion)==='function' && contador>0){
            this.props.accion();
        }

     

        return (
            <div>
                <div className="row-fluid">
                    <div className="alert alert-success" ref={this.areaMensajes} role="alert" id="msgSuccessUpload" style={areaMensajesStyle}>
                        Has seleccionado los siguientes ficheros:
                        { resultado.map((value,index) =>{
                            return (

                                <div key={index}>
                                    <li> {value.nombre} con tamaño {value.tamano} 
                                       &nbsp; <img src={value.imagen} height="20" width="20" alt={value.txtImagen} title={value.txtImagen}/>                                    
                                    </li> 
                                </div>
                            )
                        })}

                    </div>
                </div>
            </div>
        );
    }
}
export default FileList
