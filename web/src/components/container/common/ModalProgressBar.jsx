import React from 'react';
import { Modal } from "react-bootstrap";
import Spinner from 'react-bootstrap/Spinner'

/**
 * Componente que muestra en una ventana modal de bootstrap, una barra de progreso
 * @author <a href="mailto:oscarrbr@ext.inditex.com">Óscar Rodríguez</a>
 */
class ModalProgressBar extends React.Component {

    constructor(props) {
        super(props);
        this.onClose = this.onClose.bind(this);
    }

    onClose() {

    }

    render() {
        const mensaje = (this.props.mensaje!==undefined)?this.props.mensaje:"Cargando ...";
        const mostrar = (this.props.show!==undefined && this.props.show===true)?true:false;
        
        return (

            <Modal show={mostrar} onHide={this.onClose} animation={false}>
                <Modal.Header>
                    <Modal.Title>{mensaje}</Modal.Title>
                </Modal.Header>
                            
                <Modal.Body>
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />
                    <Spinner animation="border" variant="success" />                                    
                </Modal.Body>    
            </Modal>
        );
    }
}

export default ModalProgressBar;