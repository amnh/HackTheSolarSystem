pro read_spectrum, file, energy, counts, mca

    mca = obj_new('mca')
    mca->read_file, file
    energy = mca->get_energy()
    counts = mca->get_data()
end
