import React from 'react';
import Modal from 'react-bootstrap/Modal';
import Button from 'react-bootstrap/Button';

/**
 * Clase ModalConfirm que muestra un cuadro de diálogo de tipo confirmación.
 * 
 * Dispondrá de dos botones [Aceptar] y [Cancelar]
 */
class ModalConfirmation extends React.Component{

  constructor(props) {
    super(props);
    this.state = {
      show: (this.props.show==="true")?true:false,
      textButtonConfirm: (this.props.textButtonConfirm!==undefined && this.props.textButtonConfirm!=='')?this.props.textButtonConfirm:"Aceptar",
      textButtonCancel:(this.props.textButtonCancel!==undefined && this.props.textButtonCancel!=='')?this.props.textButtonCancel:"Cancelar"
    };

    this.onHide = this.onHide.bind(this);
    this.onConfirm = this.onConfirm.bind(this);
  }

  
  /**
   * Método que se ejecuta sólo cuando cambian las propiedades del componente.
   * No es invocado la primera vez en la que se renderiza el componentes
   * @param {Props} nextProps 
   */
  componentWillReceiveProps(nextProps) {
    if(this.state.show!==nextProps.show){
      this.setState({show: nextProps.show})
    }
  }

  /**
   * Cierra la ventana modal
   */
  onHide() {
    this.setState({
      show:false
    });
  }


  /**
   * 
   */
  onConfirm() {
    // Se oculta la ventana modal
    this.setState({
      show:false
    });

    if(this.props.onConfirm!==undefined && this.props.onConfirm!==null) {
      this.props.onConfirm();
    }
  }

  /**
   * Método encargado de renderizar la vista
   */
  render() {
    
      return (
          <Modal
            show={this.state.show}
            size="lg"
            aria-labelledby="contained-modal-title-vcenter"
            centered>
            <Modal.Header closeButton onClick={this.onHide}>
              <Modal.Title id="contained-modal-title-vcenter">
                {this.props.title}
              </Modal.Title>
            </Modal.Header>
            <Modal.Body>
              <p>
                  {this.props.message}
              </p>
            </Modal.Body>
            <Modal.Footer>
              <Button onClick={this.onConfirm}>{this.state.textButtonConfirm}</Button>  
              <Button onClick={this.onHide}>{this.state.textButtonCancel}</Button>
            </Modal.Footer>
          </Modal>
        );
  }
}

export default ModalConfirmation;