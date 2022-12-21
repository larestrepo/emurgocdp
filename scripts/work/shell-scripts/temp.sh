# declare -i VALIDITY
read -p 'Is the script constraint by deadline or time? [Y/N]: ' input
case $input in
    [yY][eE][sS]|[yY])
        echo "You say Yes"
        echo "[X, Y) validity range in slots; Y is going to be 100 slots more"
        read -p 'Input the starting validity slot number (X): ' VALIDITY
        read -p 'Input the number of epochs for validity: ' EPOCHS_VALID
        ;;
    [nN][oO]|[nN])
        echo "You say No"
        break
        ;;
    *)
        echo "Invalid input..."
        exit 1
        ;;
esac

HEREAFTER=$((VALIDITY+EPOCHS_VALID))
echo $HEREAFTER