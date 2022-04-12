import React from 'react';
import ReactDOM from 'react-dom';
import Navbar from 'react-bootstrap/Navbar'
import Nav from 'react-bootstrap/Nav'
import NavDropdown from 'react-bootstrap/NavDropdown'
import Button from 'react-bootstrap/Button'
import Form from 'react-bootstrap/Form'
import FormControl from 'react-bootstrap/FormControl'
import {AlmacenFacade} from '../../store/AlmacenFacade'
import WindowModalLogin from '../login/WindowModalLogin';
import { NavLink } from 'react-router-dom';


/**
 * Componente que crea un NavBar de bootstrap
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a>
 */
class BarraNavegacion extends React.Component {

    /**
     * Constructor
     * @param {Properties} props 
     */
    constructor(props) {
        super(props);
        this.actualizarBarraNavegacion = this.actualizarBarraNavegacion.bind(this);
        this.iniciarSesion = this.iniciarSesion.bind(this);
        this.cerrarSesion  = this.cerrarSesion.bind(this);
        this.state = {
            usuarioAutenticado: false
        }
    }

    /**
     * Método componentDidMount
     */
    componentDidMount() {        
    }


    /**
     * Este método es invocado desde la ventana modal de login para indicar a la barra de navegación que debe actualizarse
     */
    actualizarBarraNavegacion() {        
        this.setState({
            usuarioAutenticado: true
        });
    }

    /**
     * Método que se invoca cuando el usuario hace click sobre el enlace "Iniciar sesión" y muestra la
     * la ventana modal de login
     */
    iniciarSesion() {
        // Se renderiza el componente WindowModalLogin indicando en la propiedad que se debe mostrar
        ReactDOM.render(<WindowModalLogin actualizarMenu={this.actualizarBarraNavegacion} mostrar="true"/>, document.getElementById('ventanaModal'));
    }

    /**
     * Cierra la sesión del usuario
     */
    cerrarSesion() {
        AlmacenFacade.dispatchCerrarSesionUsuarioAction();
        this.setState({
            usuarioAutenticado: false
        });
        window.location.href="/inicio";
    }

    /**
     * Devuelve el código JSX que renderiza la Navbar que se muestra cuando el 
     * usuario no se ha autenticado
     */
    getMenuSinAutenticacion() { 
        return (
            <div>
                <Navbar bg="dark" variant="dark" expand="lg">
                <NavLink to="/inicio" className="tituloWeb">ORB SOFT</NavLink>
                <Navbar.Toggle aria-controls="basic-navbar-nav" />


                <Navbar.Collapse id="basic-navbar-nav">
                    <Nav className="mr-auto">
                    <NavLink exact to="/inicio" className="nav-link">Inicio</NavLink>
                    <NavLink exact to="/p_videos" className="nav-link">Videotecas</NavLink>

                    <NavLink exact to="/p_albumes" className="nav-link">Álbumes</NavLink>
                        <NavDropdown title="Idioma" id="basic-nav-dropdown">
                            <NavLink exact to="/" className="dropdown-item">Español</NavLink>
                            <NavDropdown.Divider />
                            <NavLink exact to="/" className="dropdown-item">Ingles</NavLink>
                        </NavDropdown>

                        <Nav.Link href="#home" onClick={this.iniciarSesion}>Iniciar sesión</Nav.Link>
                    </Nav>
                    <Form inline>
                    <FormControl type="text" placeholder="Buscar" className="mr-sm-2" />
                    <Button variant="outline-success">Buscar</Button>
                    </Form>
                </Navbar.Collapse>
                </Navbar>
            </div>
        );
    }


    /**
     * Devuelve el código JSX que renderiza la Navbar que se muestra cuando el 
     * usuario se ha autenticado
     */
    getMenuConAutenticacion() { 
        return (
            <div>
                <Navbar bg="dark" variant="dark" expand="lg">
                <NavLink to="/inicio" className="tituloWeb">ORB SOFT</NavLink>
                
                <Navbar.Toggle aria-controls="basic-navbar-nav" />
                <Navbar.Collapse id="basic-navbar-nav">
                    <Nav className="mr-auto">
                        <Nav.Link href="/">Inicio</Nav.Link>
                    
                        <NavDropdown title="Vídeos" id="basic-nav-dropdown">
                            <NavLink exact to="/pr_videoteca" className="dropdown-item">Nuevo</NavLink>

                            <NavDropdown.Divider />
                            <NavDropdown.Item href="/pr_videotecas">Tus videotecas</NavDropdown.Item>
                        </NavDropdown>

                        <NavDropdown title="Álbum" id="basic-nav-dropdown">
                            <NavLink exact to="/pr_album_nuevo" className="dropdown-item">Nuevo</NavLink>
                            <NavDropdown.Divider />
                            <NavLink exact to="/pr_albumes" className="dropdown-item">Tus álbumes</NavLink>
                        </NavDropdown>

                        <NavDropdown title="Idioma" id="basic-nav-dropdown">
                            <NavDropdown.Item href="#action/3.1">Español</NavDropdown.Item>
                            <NavDropdown.Divider />
                            <NavDropdown.Item href="#action/3.2">Ingles</NavDropdown.Item>
                        </NavDropdown>

                        <Nav.Link onClick={this.cerrarSesion}>Cerrar sesión</Nav.Link>

                    </Nav>
                    <Form inline>
                    <FormControl type="text" placeholder="Buscar" className="mr-sm-2" />
                    <Button variant="outline-success">Buscar</Button>
                    </Form>
                </Navbar.Collapse>
                </Navbar>
            </div>
        );
    }
 
    /**
     * Método que renderiza la vista
     */
    render() { 
        if(AlmacenFacade.isUserAuthenticated()) {
            return this.getMenuConAutenticacion();
        }else {
            return this.getMenuSinAutenticacion();
        }
    }

};

export default BarraNavegacion;