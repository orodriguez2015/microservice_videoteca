import React from 'react'
import { Button, Form } from 'react-bootstrap'
import { VideotecasFacade } from '../../../../facade/VideotecasFacade'
import { AlmacenFacade } from '../../../../store/AlmacenFacade'
import { StringUtil } from '../../../../util/StringUtil'
import ErrorMessage from '../../../error/ErrorMessage'
import ComponenteAutenticado from '../autenticacion/ComponenteAutenticado'
import Breadcrumb from 'react-bootstrap/Breadcrumb'
import {HTTP_OK} from '../../../../constantes/HttpResponse';

/**
 * Componente EditarVideoteca que permite editar una videoteca
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class EditarVideoteca extends ComponenteAutenticado {
  /**
   * Constructor
   * @param {Properties} props
   */
  constructor(props) {
    super(props)
    this.nombre = React.createRef()
    this.areaMensajeError = React.createRef()
    this.publico = React.createRef()
    this.idVideoteca = React.createRef()
    this.handleAceptar = this.handleAceptar.bind(this)
    this.pulsarTecla = this.pulsarTecla.bind(this)
    this.handleChangeNombre = this.handleChangeNombre.bind(this)
    this.handleChangeCarpeta = this.handleChangeCarpeta.bind(this)

    this.state = {
      nombre: '',
      error: false,
      descError: '',
    }
  }

  /**
   * Método que se invoca una vez que se ha montado el componente.
   * En este método se recuperan los datos
   */
  componentDidMount() {
    if (this.estaUsuarioAutenticado()) {
      // Se comprueba si se ha pasado el parámetro p_videoteca_id en la url.
      if (
        this.props.match.params === null ||
        this.props.match.params.p_videoteca_id === undefined
      ) {
        this.setEstado([], '', true, 'Videoteca desconocida')
      } else {
        // Se recupera el id del videoteca pasado como parámetro en la url
        var idVideoteca = this.props.match.params.p_videoteca_id
        // Se recupera el nombre de la videoteca pasada en el estado en la petición de carga del componente


        if (StringUtil.isNotEmpty(idVideoteca)) {
          VideotecasFacade.getVideoteca(idVideoteca, 1)
            .then((resultado) => {
              if (resultado.codStatus === HTTP_OK) {
                this.idVideoteca.current.value = resultado.data.id
                this.nombre.current.value = resultado.data.nombre

                if (resultado.data.publico === true) {
                  this.publico.current.checked = true
                }
              } else {
                this.setEstado(
                  true,
                  'Se ha producido un error al recuperar la videoteca a modificar',
                )
              }
            })
            .catch((err) => {
              this.setEstado(
                true,
                'Se ha producido un error al recuperar la videoteca a modificar',
              )
            })
        } else {
          this.setEstado(true, 'Videoteca desconocida')
        }
      }
    }
  }

  /**
   * Método setEstado que permite modificar el estado del componente
   * @param {Boolean} error True si hay error y false en caso contrario
   * @param {String} descripcionError Descripcion del error en caso de que lo hubiese
   */
  setEstado(error, descripcionError) {
    this.setState({
      error: error,
      descError: StringUtil.isNotEmpty(descripcionError)
        ? descripcionError
        : 'Videoteca desconocida',
    })
  }

  /**
   * Manejador asociado al botón [Aceptar]
   * @param {Event} evt
   */
  handleAceptar(evt) {
    evt.preventDefault()
    this.editarVideoteca();
   
  }

  /**
   * Método que se invoca para enviar la petición de edicion de una videoteca al servidor
   */
  editarVideoteca() {
    let user = AlmacenFacade.getUser()

    VideotecasFacade.updateVideoteca(
      this.nombre.current.value,
      this.publico.current.checked,
      this.idVideoteca.current.value
    )
      .then((resultado) => {

        if(resultado.codStatus===201) {
          // Se redirige a la pantalla de administración de videotecas
          this.props.history.push('/pr_videotecas')
        } else {
          this.mostrarMensajeError(
            'Se ha producido un error al editar la videoteca. Intentelo de nuevo',
          );

        };

      })
      .catch((err) => {
        this.mostrarMensajeError(
          'Se ha producido un error genérico al editar la videoteca. Intentelo de nuevo',
        )
      })
  }

  /**
   * Manejador asociado al botón "Cerrar"
   * @param {Event} evt
   */
  handleClose(evt) {
    evt.preventDefault()
    window.location.href = '/pr_videotecas'
  }

  /**
   * Muestra un mensaje en el área dedicada a mensajes de error
   * @param {String} msg  Mensaje de error
   */
  mostrarMensajeError(msg) {
    if (StringUtil.isNotEmpty(msg)) {
      this.areaMensajeError.current.innerHTML = msg
    }
  }

  /**
   * Limpia el área de mensajes de error de cualquier contenido
   */
  limpiarMensajeError() {
    this.areaMensajeError.current.innerHTML = ''
  }

  /**
   * Método que se invoca cuando es pulsada una tecla. La acción que
   * se desencadena es la de borrar el área de mensajes de error
   */
  pulsarTecla() {
    this.limpiarMensajeError()
  }

  /**
   * Manejador para el evento change asociado al campo de formulario "Nombre"
   * @param {Event} evt
   */
  handleChangeNombre(evt) {
    evt.preventDefault()
    this.limpiarMensajeError()
  }

  /**
   * Manejador para el evento change asociado al campo de formulario "Carpeta"
   * @param {Event} evt
   */
  handleChangeCarpeta(evt) {
    evt.preventDefault()
    this.limpiarMensajeError()
  }

  /**
   * Método que renderiza la vista
   */
  render() {
    var estiloMsgError = {
      color: 'red',
      fontSize: '15px',
      textAlign: 'center',
    }

    if (this.hayErrores()) {
      return <ErrorMessage mensaje={this.getMensajeError()} />
    } else {
      return (
        <div className="container">
          <div className="subtitulo">
            <h2>Edición de videoteca</h2>
            <hr></hr>
          </div>

          <Breadcrumb>
            <Breadcrumb.Item href="/">Inicio</Breadcrumb.Item>
            <Breadcrumb.Item href="/pr_videotecas">Videotecas</Breadcrumb.Item>
            <Breadcrumb.Item href={'/pr_videoteca/' + this.props.match.params.p_videoteca_id}>              
                Detalle              
            </Breadcrumb.Item>
            <Breadcrumb.Item href={'/pr_video_adjuntar/' + this.props.match.params.p_videoteca_id}>              
                Adjuntar vídeo
            </Breadcrumb.Item>
            <Breadcrumb.Item active>Edición</Breadcrumb.Item>
          </Breadcrumb>

          <div className="row d-flex">
            <div className="center">
              <Form onSubmit={this.handleAceptar}>
                <Form.Group controlId="formGridNombre">
                  <div className="form-group">
                    <Form.Label align="left">Nombre *</Form.Label>
                    <div className="col-sm-10">
                      <input
                        type="text"
                        ref={this.nombre}
                        className="form-control"
                        size="50"
                        id="nombre"
                        name="nombre"
                        placeholder="Nombre de la videoteca"
                        required="required"
                        onChange={this.handleChangeNombre}
                        autoFocus
                      />
                      <input
                        type="hidden"
                        ref={this.idVideoteca}
                        id="idVideoteca"
                      />
                    </div>
                  </div>
                </Form.Group>

        

                <Form.Group controlId="formBasicCheckbox">
                  <Form.Check
                    type="checkbox"
                    ref={this.publico}
                    label="Público"
                  />
                </Form.Group>

                <div align="right">
                  <Button variant="success" type="submit">
                    Aceptar
                  </Button>
                  &nbsp;&nbsp;
                  <Button variant="danger" onClick={this.handleClose}>
                    Cancelar
                  </Button>
                </div>
              </Form>
            </div>
          </div>
          <p></p>
          <div ref={this.areaMensajeError} style={estiloMsgError} />
        </div>
      )
    }
  }
}

export default EditarVideoteca
