import React from 'react';
import { Route,Router } from 'react-router-dom';
import Inicio from '../container/Inicio';
import VideotecasPublicas from '../container/publico/VideotecasPublicas';
import AlbumesPublicos from '../container/publico/AlbumesPublicos';
import DetalleAlbumPublico from '../container/publico/DetalleAlbumPublico';
import DetalleVideotecaPublica from '../container/publico/DetalleVideotecaPublica';
import AlbumForm from '../container/privado/album/AlbumForm';
import AlbumesPrivados from '../container/privado/album/AlbumesPrivados';
import EditarAlbumForm from '../container/privado/album/EditarAlbumForm';
import DetalleAlbumPrivado from '../container/privado/album/DetalleAlbumPrivado';
import VideotecasPrivadas from '../container/privado/videoteca/VideotecasPrivadas';
import DetalleVideoteca from '../container/privado/videoteca/DetalleVideoteca';
import VideotecaForm from '../container/privado/videoteca/VideotecaForm';
import EditarVideoteca from '../container/privado/videoteca/EditarVideoteca';
import ErrorMessage from '../error/ErrorMessage';
import BarraNavegacion from '../menu/BarraNavegacion';
import AdjuntarVideo from '../container/privado/videoteca/AdjuntarVideo';
import AdjuntarFoto from '../container/privado/album/AdjuntarFoto';

/**
 * La versión de react-router-dom es la 4.3.1 porque las más reciente de la 5.X en adelante
 * no funciona con Route,Router y history.createBrowserHistory()
 */
var history = require("history").createBrowserHistory();

/**
 * En este clase se configuran las URL correspondientes a cada componente y se renderiza 
 * la barra de navegación, desde la cual se renderiza cada uno de los componentes, tanto los
 * correspondientes a la parte pública como los del área privada
 * 
 * @author <a href="mailto:oscar.rodriguezbrea@gmail.com">Óscar Rodríguez</a> 
 */
class RoutesConfig extends React.Component {

    /**
     * Renderiza el componente
     * <Route exact={true} path="/pr_album/:p_album_id" component={EditarAlbumForm} />                    
     */
    render() {
        return (
            <div>
                <Router history={history}>      
                <div>  
                    <BarraNavegacion/>
                    <Route exact={true} path="/inicio" component={Inicio} />
                    <Route exact={true} path="/p_videos" component={VideotecasPublicas} />
                    <Route exact={true} path="/p_videos/:p_videoteca_id" component={DetalleVideotecaPublica} />
                    <Route exact={true} path="/p_albumes" component={AlbumesPublicos} />
                    <Route exact={true} path="/p_album/:p_album_id" name="p_album" component={DetalleAlbumPublico} />
                    <Route exact={true} path="/pr_album_nuevo" component={AlbumForm} />
                    
                    <Route exact={true} path="/pr_albumes" component={AlbumesPrivados} />
                    <Route exact={true} path="/pr_album/:p_album_id" component={DetalleAlbumPrivado} />
                    <Route exact={true} path="/pr_album/edit/:p_album_id" component={EditarAlbumForm} />
                    <Route exact={true} path="/pr_album/adjuntar/:p_album_id" component={AdjuntarFoto} />

                    <Route exact={true} path="/pr_videoteca" component={VideotecaForm} />
                    <Route exact={true} path="/pr_videotecas" component={VideotecasPrivadas} />
                    <Route exact={true} path="/pr_videoteca/:p_videoteca_id" component={DetalleVideoteca} />
                    <Route exact={true} path="/pr_videoteca/edit/:p_videoteca_id" component={EditarVideoteca} />


                    <Route exact={true} path="/pr_video_adjuntar/:p_videoteca_id" component={AdjuntarVideo} />
                    <Route exact={true} path="/error" component={ErrorMessage} />
                </div>
                </Router>
              
            </div>
        )
    }
}


export default RoutesConfig;