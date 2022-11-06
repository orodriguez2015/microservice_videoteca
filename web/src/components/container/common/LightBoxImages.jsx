import React from 'react';
import ReactDOM from 'react-dom';
import Lightbox from "react-image-lightbox";
import 'react-image-lightbox/style.css'; 


/**
 * Component LightBox que muestra un LightBox de un conjunto de imágenes
 * que se le pasen a través de las propiedades del componente
 */
class LightBoxImages extends React.Component {

    constructor(props) {
        super(props);

        this.state = {
            photoIndex: this.props.photoIndex,
            images:this.props.images
        };

        this.onCloseLightBox = this.onCloseLightBox.bind(this);

        // Array que contiene los ids de las imágenes que se van visualizando y que se han
        // pasado por parámetro al LightBox
        this.seenImages = [];
    }


    /**
     * Comprueba si la imagen se encuentre entre las imagenes ya visualizados.
     * Si ya se ha visualizado, se evita que se incremente el contador de visualizaciones de la foto 
     * @param {Integer} id Id de la imagen 
     * @returns {Boolean}
     */
    isImageHasBeenViewed(id) {
        var exito = false;
        if(this.seenImages.includes(id)===true) {
            exito = true;
        }
        return exito;
    }


    /**
     * Inserta la imagen en el array de imágenes ya visualizadas
     * @param {Integer} Id de la imagen
     */
    insertImageViewed(id){
        this.seenImages.push(id);
    }


    /**
     * Callback que es llamado cuando Se envia petición al servidor
     * @param {Integer} Id de la imagen
     */
     onCallbackImageLoad(id) {

        if(id!==undefined && this.isImageHasBeenViewed(id)===false && typeof(this.props.onCallbackActivateCounterDisplay)==="function") {
            // La imagen ha sido seleccionada por el usuario y se añade 
            this.seenImages.push(id);
            this.props.onCallbackActivateCounterDisplay(id);
        }
    }

    /**
     * Se ejecuta cuando se cierra el lightbox
     */
    onCloseLightBox() {
        this.destroyComponent();
    }

    /**
     * Desmonta el componente que será invocado cuando se cierra el LightBox
     */
    destroyComponent() {
        var container = ReactDOM.findDOMNode(this).parentNode;
        ReactDOM.unmountComponentAtNode(container);
    }


    /**
     * Renderiza la vista
     */
    render() {
        const { photoIndex, images } = this.state;
        
        return (
            <div>
            
                {(
                <Lightbox
                    mainSrc={images[photoIndex].url}
                    nextSrc={images[(photoIndex + 1) % images.length].url}
                    prevSrc={images[(photoIndex + images.length - 1) % images.length].url}
                    onCloseRequest={() => this.onCloseLightBox()}
                    onMovePrevRequest={() =>
                    this.setState({
                        photoIndex: (photoIndex + images.length - 1) % images.length,
                    })
                    }
                    onMoveNextRequest={() =>
                    this.setState({
                        photoIndex: (photoIndex + 1) % images.length,
                    })
                    }

                    onImageLoad={
                       (imageSrc,srcType,image)=> this.onCallbackImageLoad(images[photoIndex].id)                       
                    }
                   
                   
                />
                )}
            </div>
        );
    }

}

export default LightBoxImages;